// sndjvu_codec::zp
// Copyright (C) 2021 Cole Miller
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

/*! Implements the Z&prime;-Coder, an approximate binary arithmetic coder.

The Z&prime;-Coder is used as a primitive by the higher-level compression algorithms implemented by
this library.
*/

#[derive(Clone, Copy, Debug)]
struct Entry {
    delta: u16,
    theta: u16,
    mu: u8,
    lambda: u8,
}

#[rustfmt::skip]
static TABLE: [Entry; 251] = [
    Entry { delta: 0x8000, theta: 0x0000, mu:  84, lambda: 145 },
    Entry { delta: 0x8000, theta: 0x0000, mu:   3, lambda:   4 },
    Entry { delta: 0x8000, theta: 0x0000, mu:   4, lambda:   3 },
    Entry { delta: 0x6bbd, theta: 0x10a5, mu:   5, lambda:   1 },
    Entry { delta: 0x6bbd, theta: 0x10a5, mu:   6, lambda:   2 },
    Entry { delta: 0x5d45, theta: 0x1f28, mu:   7, lambda:   3 },
    Entry { delta: 0x5d45, theta: 0x1f28, mu:   8, lambda:   4 },
    Entry { delta: 0x51b9, theta: 0x2bd3, mu:   9, lambda:   5 },
    Entry { delta: 0x51b9, theta: 0x2bd3, mu:  10, lambda:   6 },
    Entry { delta: 0x4813, theta: 0x36e3, mu:  11, lambda:   7 },
    Entry { delta: 0x4813, theta: 0x36e3, mu:  12, lambda:   8 },
    Entry { delta: 0x3fd5, theta: 0x408c, mu:  13, lambda:   9 },
    Entry { delta: 0x3fd5, theta: 0x408c, mu:  14, lambda:  10 },
    Entry { delta: 0x38b1, theta: 0x48fd, mu:  15, lambda:  11 },
    Entry { delta: 0x38b1, theta: 0x48fd, mu:  16, lambda:  12 },
    Entry { delta: 0x3275, theta: 0x505d, mu:  17, lambda:  13 },
    Entry { delta: 0x3275, theta: 0x505d, mu:  18, lambda:  14 },
    Entry { delta: 0x2cfd, theta: 0x56d0, mu:  19, lambda:  15 },
    Entry { delta: 0x2cfd, theta: 0x56d0, mu:  20, lambda:  16 },
    Entry { delta: 0x2825, theta: 0x5c71, mu:  21, lambda:  17 },
    Entry { delta: 0x2825, theta: 0x5c71, mu:  22, lambda:  18 },
    Entry { delta: 0x23ab, theta: 0x615b, mu:  23, lambda:  19 },
    Entry { delta: 0x23ab, theta: 0x615b, mu:  24, lambda:  20 },
    Entry { delta: 0x1f87, theta: 0x65a5, mu:  25, lambda:  21 },
    Entry { delta: 0x1f87, theta: 0x65a5, mu:  26, lambda:  22 },
    Entry { delta: 0x1bbb, theta: 0x6962, mu:  27, lambda:  23 },
    Entry { delta: 0x1bbb, theta: 0x6962, mu:  28, lambda:  24 },
    Entry { delta: 0x1845, theta: 0x6ca2, mu:  29, lambda:  25 },
    Entry { delta: 0x1845, theta: 0x6ca2, mu:  30, lambda:  26 },
    Entry { delta: 0x1523, theta: 0x6f74, mu:  31, lambda:  27 },
    Entry { delta: 0x1523, theta: 0x6f74, mu:  32, lambda:  28 },
    Entry { delta: 0x1253, theta: 0x71e6, mu:  33, lambda:  29 },
    Entry { delta: 0x1253, theta: 0x71e6, mu:  34, lambda:  30 },
    Entry { delta: 0x0fcf, theta: 0x7404, mu:  35, lambda:  31 },
    Entry { delta: 0x0fcf, theta: 0x7404, mu:  36, lambda:  32 },
    Entry { delta: 0x0d95, theta: 0x75d6, mu:  37, lambda:  33 },
    Entry { delta: 0x0d95, theta: 0x75d6, mu:  38, lambda:  34 },
    Entry { delta: 0x0b9d, theta: 0x7768, mu:  39, lambda:  35 },
    Entry { delta: 0x0b9d, theta: 0x7768, mu:  40, lambda:  36 },
    Entry { delta: 0x09e3, theta: 0x78c2, mu:  41, lambda:  37 },
    Entry { delta: 0x09e3, theta: 0x78c2, mu:  42, lambda:  38 },
    Entry { delta: 0x0861, theta: 0x79ea, mu:  43, lambda:  39 },
    Entry { delta: 0x0861, theta: 0x79ea, mu:  44, lambda:  40 },
    Entry { delta: 0x0711, theta: 0x7ae7, mu:  45, lambda:  41 },
    Entry { delta: 0x0711, theta: 0x7ae7, mu:  46, lambda:  42 },
    Entry { delta: 0x05f1, theta: 0x7bbe, mu:  47, lambda:  43 },
    Entry { delta: 0x05f1, theta: 0x7bbe, mu:  48, lambda:  44 },
    Entry { delta: 0x04f9, theta: 0x7c75, mu:  49, lambda:  45 },
    Entry { delta: 0x04f9, theta: 0x7c75, mu:  50, lambda:  46 },
    Entry { delta: 0x0425, theta: 0x7d0f, mu:  51, lambda:  47 },
    Entry { delta: 0x0425, theta: 0x7d0f, mu:  52, lambda:  48 },
    Entry { delta: 0x0371, theta: 0x7d91, mu:  53, lambda:  49 },
    Entry { delta: 0x0371, theta: 0x7d91, mu:  54, lambda:  50 },
    Entry { delta: 0x02d9, theta: 0x7dfe, mu:  55, lambda:  51 },
    Entry { delta: 0x02d9, theta: 0x7dfe, mu:  56, lambda:  52 },
    Entry { delta: 0x0259, theta: 0x7e5a, mu:  57, lambda:  53 },
    Entry { delta: 0x0259, theta: 0x7e5a, mu:  58, lambda:  54 },
    Entry { delta: 0x01ed, theta: 0x7ea6, mu:  59, lambda:  55 },
    Entry { delta: 0x01ed, theta: 0x7ea6, mu:  60, lambda:  56 },
    Entry { delta: 0x0193, theta: 0x7ee6, mu:  61, lambda:  57 },
    Entry { delta: 0x0193, theta: 0x7ee6, mu:  62, lambda:  58 },
    Entry { delta: 0x0149, theta: 0x7f1a, mu:  63, lambda:  59 },
    Entry { delta: 0x0149, theta: 0x7f1a, mu:  64, lambda:  60 },
    Entry { delta: 0x010b, theta: 0x7f45, mu:  65, lambda:  61 },
    Entry { delta: 0x010b, theta: 0x7f45, mu:  66, lambda:  62 },
    Entry { delta: 0x00d5, theta: 0x7f6b, mu:  67, lambda:  63 },
    Entry { delta: 0x00d5, theta: 0x7f6b, mu:  68, lambda:  64 },
    Entry { delta: 0x00a5, theta: 0x7f8d, mu:  69, lambda:  65 },
    Entry { delta: 0x00a5, theta: 0x7f8d, mu:  70, lambda:  66 },
    Entry { delta: 0x007b, theta: 0x7faa, mu:  71, lambda:  67 },
    Entry { delta: 0x007b, theta: 0x7faa, mu:  72, lambda:  68 },
    Entry { delta: 0x0057, theta: 0x7fc3, mu:  73, lambda:  69 },
    Entry { delta: 0x0057, theta: 0x7fc3, mu:  74, lambda:  70 },
    Entry { delta: 0x003b, theta: 0x7fd7, mu:  75, lambda:  71 },
    Entry { delta: 0x003b, theta: 0x7fd7, mu:  76, lambda:  72 },
    Entry { delta: 0x0023, theta: 0x7fe7, mu:  77, lambda:  73 },
    Entry { delta: 0x0023, theta: 0x7fe7, mu:  78, lambda:  74 },
    Entry { delta: 0x0013, theta: 0x7ff2, mu:  79, lambda:  75 },
    Entry { delta: 0x0013, theta: 0x7ff2, mu:  80, lambda:  76 },
    Entry { delta: 0x0007, theta: 0x7ffa, mu:  81, lambda:  77 },
    Entry { delta: 0x0007, theta: 0x7ffa, mu:  82, lambda:  78 },
    Entry { delta: 0x0001, theta: 0x7fff, mu:  81, lambda:  79 },
    Entry { delta: 0x0001, theta: 0x7fff, mu:  82, lambda:  80 },
    Entry { delta: 0x5695, theta: 0x0000, mu:   9, lambda:  85 },
    Entry { delta: 0x24ee, theta: 0x0000, mu:  86, lambda: 226 },
    Entry { delta: 0x8000, theta: 0x0000, mu:   5, lambda:   6 },
    Entry { delta: 0x0d30, theta: 0x0000, mu:  88, lambda: 176 },
    Entry { delta: 0x481a, theta: 0x0000, mu:  89, lambda: 143 },
    Entry { delta: 0x0481, theta: 0x0000, mu:  90, lambda: 138 },
    Entry { delta: 0x3579, theta: 0x0000, mu:  91, lambda: 141 },
    Entry { delta: 0x017a, theta: 0x0000, mu:  92, lambda: 112 },
    Entry { delta: 0x24ef, theta: 0x0000, mu:  93, lambda: 135 },
    Entry { delta: 0x007b, theta: 0x0000, mu:  94, lambda: 104 },
    Entry { delta: 0x1978, theta: 0x0000, mu:  95, lambda: 133 },
    Entry { delta: 0x0028, theta: 0x0000, mu:  96, lambda: 100 },
    Entry { delta: 0x10ca, theta: 0x0000, mu:  97, lambda: 129 },
    Entry { delta: 0x000d, theta: 0x0000, mu:  82, lambda:  98 },
    Entry { delta: 0x0b5d, theta: 0x0000, mu:  99, lambda: 127 },
    Entry { delta: 0x0034, theta: 0x0000, mu:  76, lambda:  72 },
    Entry { delta: 0x078a, theta: 0x0000, mu: 101, lambda: 125 },
    Entry { delta: 0x00a0, theta: 0x0000, mu:  70, lambda: 102 },
    Entry { delta: 0x050f, theta: 0x0000, mu: 103, lambda: 123 },
    Entry { delta: 0x0117, theta: 0x0000, mu:  66, lambda:  60 },
    Entry { delta: 0x0358, theta: 0x0000, mu: 105, lambda: 121 },
    Entry { delta: 0x01ea, theta: 0x0000, mu: 106, lambda: 110 },
    Entry { delta: 0x0234, theta: 0x0000, mu: 107, lambda: 119 },
    Entry { delta: 0x0144, theta: 0x0000, mu:  66, lambda: 108 },
    Entry { delta: 0x0173, theta: 0x0000, mu: 109, lambda: 117 },
    Entry { delta: 0x0234, theta: 0x0000, mu:  60, lambda:  54 },
    Entry { delta: 0x00f5, theta: 0x0000, mu: 111, lambda: 115 },
    Entry { delta: 0x0353, theta: 0x0000, mu:  56, lambda:  48 },
    Entry { delta: 0x00a1, theta: 0x0000, mu:  69, lambda: 113 },
    Entry { delta: 0x05c5, theta: 0x0000, mu: 114, lambda: 134 },
    Entry { delta: 0x011a, theta: 0x0000, mu:  65, lambda:  59 },
    Entry { delta: 0x03cf, theta: 0x0000, mu: 116, lambda: 132 },
    Entry { delta: 0x01aa, theta: 0x0000, mu:  61, lambda:  55 },
    Entry { delta: 0x0285, theta: 0x0000, mu: 118, lambda: 130 },
    Entry { delta: 0x0286, theta: 0x0000, mu:  57, lambda:  51 },
    Entry { delta: 0x01ab, theta: 0x0000, mu: 120, lambda: 128 },
    Entry { delta: 0x03d3, theta: 0x0000, mu:  53, lambda:  47 },
    Entry { delta: 0x011a, theta: 0x0000, mu: 122, lambda: 126 },
    Entry { delta: 0x05c5, theta: 0x0000, mu:  49, lambda:  41 },
    Entry { delta: 0x00ba, theta: 0x0000, mu: 124, lambda:  62 },
    Entry { delta: 0x08ad, theta: 0x0000, mu:  43, lambda:  37 },
    Entry { delta: 0x007a, theta: 0x0000, mu:  72, lambda:  66 },
    Entry { delta: 0x0ccc, theta: 0x0000, mu:  39, lambda:  31 },
    Entry { delta: 0x01eb, theta: 0x0000, mu:  60, lambda:  54 },
    Entry { delta: 0x1302, theta: 0x0000, mu:  33, lambda:  25 },
    Entry { delta: 0x02e6, theta: 0x0000, mu:  56, lambda:  50 },
    Entry { delta: 0x1b81, theta: 0x0000, mu:  29, lambda: 131 },
    Entry { delta: 0x045e, theta: 0x0000, mu:  52, lambda:  46 },
    Entry { delta: 0x24ef, theta: 0x0000, mu:  23, lambda:  17 },
    Entry { delta: 0x0690, theta: 0x0000, mu:  48, lambda:  40 },
    Entry { delta: 0x2865, theta: 0x0000, mu:  23, lambda:  15 },
    Entry { delta: 0x09de, theta: 0x0000, mu:  42, lambda: 136 },
    Entry { delta: 0x3987, theta: 0x0000, mu: 137, lambda:   7 },
    Entry { delta: 0x0dc8, theta: 0x0000, mu:  38, lambda:  32 },
    Entry { delta: 0x2c99, theta: 0x0000, mu:  21, lambda: 139 },
    Entry { delta: 0x10ca, theta: 0x0000, mu: 140, lambda: 172 },
    Entry { delta: 0x3b5f, theta: 0x0000, mu:  15, lambda:   9 },
    Entry { delta: 0x0b5d, theta: 0x0000, mu: 142, lambda: 170 },
    Entry { delta: 0x5695, theta: 0x0000, mu:   9, lambda:  85 },
    Entry { delta: 0x078a, theta: 0x0000, mu: 144, lambda: 168 },
    Entry { delta: 0x8000, theta: 0x0000, mu: 141, lambda: 248 },
    Entry { delta: 0x050f, theta: 0x0000, mu: 146, lambda: 166 },
    Entry { delta: 0x24ee, theta: 0x0000, mu: 147, lambda: 247 },
    Entry { delta: 0x0358, theta: 0x0000, mu: 148, lambda: 164 },
    Entry { delta: 0x0d30, theta: 0x0000, mu: 149, lambda: 197 },
    Entry { delta: 0x0234, theta: 0x0000, mu: 150, lambda: 162 },
    Entry { delta: 0x0481, theta: 0x0000, mu: 151, lambda:  95 },
    Entry { delta: 0x0173, theta: 0x0000, mu: 152, lambda: 160 },
    Entry { delta: 0x017a, theta: 0x0000, mu: 153, lambda: 173 },
    Entry { delta: 0x00f5, theta: 0x0000, mu: 154, lambda: 158 },
    Entry { delta: 0x007b, theta: 0x0000, mu: 155, lambda: 165 },
    Entry { delta: 0x00a1, theta: 0x0000, mu:  70, lambda: 156 },
    Entry { delta: 0x0028, theta: 0x0000, mu: 157, lambda: 161 },
    Entry { delta: 0x011a, theta: 0x0000, mu:  66, lambda:  60 },
    Entry { delta: 0x000d, theta: 0x0000, mu:  81, lambda: 159 },
    Entry { delta: 0x01aa, theta: 0x0000, mu:  62, lambda:  56 },
    Entry { delta: 0x0034, theta: 0x0000, mu:  75, lambda:  71 },
    Entry { delta: 0x0286, theta: 0x0000, mu:  58, lambda:  52 },
    Entry { delta: 0x00a0, theta: 0x0000, mu:  69, lambda: 163 },
    Entry { delta: 0x03d3, theta: 0x0000, mu:  54, lambda:  48 },
    Entry { delta: 0x0117, theta: 0x0000, mu:  65, lambda:  59 },
    Entry { delta: 0x05c5, theta: 0x0000, mu:  50, lambda:  42 },
    Entry { delta: 0x01ea, theta: 0x0000, mu: 167, lambda: 171 },
    Entry { delta: 0x08ad, theta: 0x0000, mu:  44, lambda:  38 },
    Entry { delta: 0x0144, theta: 0x0000, mu:  65, lambda: 169 },
    Entry { delta: 0x0ccc, theta: 0x0000, mu:  40, lambda:  32 },
    Entry { delta: 0x0234, theta: 0x0000, mu:  59, lambda:  53 },
    Entry { delta: 0x1302, theta: 0x0000, mu:  34, lambda:  26 },
    Entry { delta: 0x0353, theta: 0x0000, mu:  55, lambda:  47 },
    Entry { delta: 0x1b81, theta: 0x0000, mu:  30, lambda: 174 },
    Entry { delta: 0x05c5, theta: 0x0000, mu: 175, lambda: 193 },
    Entry { delta: 0x24ef, theta: 0x0000, mu:  24, lambda:  18 },
    Entry { delta: 0x03cf, theta: 0x0000, mu: 177, lambda: 191 },
    Entry { delta: 0x2b74, theta: 0x0000, mu: 178, lambda: 222 },
    Entry { delta: 0x0285, theta: 0x0000, mu: 179, lambda: 189 },
    Entry { delta: 0x201d, theta: 0x0000, mu: 180, lambda: 218 },
    Entry { delta: 0x01ab, theta: 0x0000, mu: 181, lambda: 187 },
    Entry { delta: 0x1715, theta: 0x0000, mu: 182, lambda: 216 },
    Entry { delta: 0x011a, theta: 0x0000, mu: 183, lambda: 185 },
    Entry { delta: 0x0fb7, theta: 0x0000, mu: 184, lambda: 214 },
    Entry { delta: 0x00ba, theta: 0x0000, mu:  69, lambda:  61 },
    Entry { delta: 0x0a67, theta: 0x0000, mu: 186, lambda: 212 },
    Entry { delta: 0x01eb, theta: 0x0000, mu:  59, lambda:  53 },
    Entry { delta: 0x06e7, theta: 0x0000, mu: 188, lambda: 210 },
    Entry { delta: 0x02e6, theta: 0x0000, mu:  55, lambda:  49 },
    Entry { delta: 0x0496, theta: 0x0000, mu: 190, lambda: 208 },
    Entry { delta: 0x045e, theta: 0x0000, mu:  51, lambda:  45 },
    Entry { delta: 0x030d, theta: 0x0000, mu: 192, lambda: 206 },
    Entry { delta: 0x0690, theta: 0x0000, mu:  47, lambda:  39 },
    Entry { delta: 0x0206, theta: 0x0000, mu: 194, lambda: 204 },
    Entry { delta: 0x09de, theta: 0x0000, mu:  41, lambda: 195 },
    Entry { delta: 0x0155, theta: 0x0000, mu: 196, lambda: 202 },
    Entry { delta: 0x0dc8, theta: 0x0000, mu:  37, lambda:  31 },
    Entry { delta: 0x00e1, theta: 0x0000, mu: 198, lambda: 200 },
    Entry { delta: 0x2b74, theta: 0x0000, mu: 199, lambda: 243 },
    Entry { delta: 0x0094, theta: 0x0000, mu:  72, lambda:  64 },
    Entry { delta: 0x201d, theta: 0x0000, mu: 201, lambda: 239 },
    Entry { delta: 0x0188, theta: 0x0000, mu:  62, lambda:  56 },
    Entry { delta: 0x1715, theta: 0x0000, mu: 203, lambda: 237 },
    Entry { delta: 0x0252, theta: 0x0000, mu:  58, lambda:  52 },
    Entry { delta: 0x0fb7, theta: 0x0000, mu: 205, lambda: 235 },
    Entry { delta: 0x0383, theta: 0x0000, mu:  54, lambda:  48 },
    Entry { delta: 0x0a67, theta: 0x0000, mu: 207, lambda: 233 },
    Entry { delta: 0x0547, theta: 0x0000, mu:  50, lambda:  44 },
    Entry { delta: 0x06e7, theta: 0x0000, mu: 209, lambda: 231 },
    Entry { delta: 0x07e2, theta: 0x0000, mu:  46, lambda:  38 },
    Entry { delta: 0x0496, theta: 0x0000, mu: 211, lambda: 229 },
    Entry { delta: 0x0bc0, theta: 0x0000, mu:  40, lambda:  34 },
    Entry { delta: 0x030d, theta: 0x0000, mu: 213, lambda: 227 },
    Entry { delta: 0x1178, theta: 0x0000, mu:  36, lambda:  28 },
    Entry { delta: 0x0206, theta: 0x0000, mu: 215, lambda: 225 },
    Entry { delta: 0x19da, theta: 0x0000, mu:  30, lambda:  22 },
    Entry { delta: 0x0155, theta: 0x0000, mu: 217, lambda: 223 },
    Entry { delta: 0x24ef, theta: 0x0000, mu:  26, lambda:  16 },
    Entry { delta: 0x00e1, theta: 0x0000, mu: 219, lambda: 221 },
    Entry { delta: 0x320e, theta: 0x0000, mu:  20, lambda: 220 },
    Entry { delta: 0x0094, theta: 0x0000, mu:  71, lambda:  63 },
    Entry { delta: 0x432a, theta: 0x0000, mu:  14, lambda:   8 },
    Entry { delta: 0x0188, theta: 0x0000, mu:  61, lambda:  55 },
    Entry { delta: 0x447d, theta: 0x0000, mu:  14, lambda: 224 },
    Entry { delta: 0x0252, theta: 0x0000, mu:  57, lambda:  51 },
    Entry { delta: 0x5ece, theta: 0x0000, mu:   8, lambda:   2 },
    Entry { delta: 0x0383, theta: 0x0000, mu:  53, lambda:  47 },
    Entry { delta: 0x8000, theta: 0x0000, mu: 228, lambda:  87 },
    Entry { delta: 0x0547, theta: 0x0000, mu:  49, lambda:  43 },
    Entry { delta: 0x481a, theta: 0x0000, mu: 230, lambda: 246 },
    Entry { delta: 0x07e2, theta: 0x0000, mu:  45, lambda:  37 },
    Entry { delta: 0x3579, theta: 0x0000, mu: 232, lambda: 244 },
    Entry { delta: 0x0bc0, theta: 0x0000, mu:  39, lambda:  33 },
    Entry { delta: 0x24ef, theta: 0x0000, mu: 234, lambda: 238 },
    Entry { delta: 0x1178, theta: 0x0000, mu:  35, lambda:  27 },
    Entry { delta: 0x1978, theta: 0x0000, mu: 138, lambda: 236 },
    Entry { delta: 0x19da, theta: 0x0000, mu:  29, lambda:  21 },
    Entry { delta: 0x2865, theta: 0x0000, mu:  24, lambda:  16 },
    Entry { delta: 0x24ef, theta: 0x0000, mu:  25, lambda:  15 },
    Entry { delta: 0x3987, theta: 0x0000, mu: 240, lambda:   8 },
    Entry { delta: 0x320e, theta: 0x0000, mu:  19, lambda: 241 },
    Entry { delta: 0x2c99, theta: 0x0000, mu:  22, lambda: 242 },
    Entry { delta: 0x432a, theta: 0x0000, mu:  13, lambda:   7 },
    Entry { delta: 0x3b5f, theta: 0x0000, mu:  16, lambda:  10 },
    Entry { delta: 0x447d, theta: 0x0000, mu:  13, lambda: 245 },
    Entry { delta: 0x5695, theta: 0x0000, mu:  10, lambda:   2 },
    Entry { delta: 0x5ece, theta: 0x0000, mu:   7, lambda:   1 },
    Entry { delta: 0x8000, theta: 0x0000, mu: 244, lambda:  83 },
    Entry { delta: 0x8000, theta: 0x0000, mu: 249, lambda: 250 },
    Entry { delta: 0x5695, theta: 0x0000, mu:  10, lambda:   2 },
    Entry { delta: 0x481a, theta: 0x0000, mu:  89, lambda: 143 },
    Entry { delta: 0x481a, theta: 0x0000, mu: 230, lambda: 246 },
];

/// A context for normal operation of the Z&prime;-Coder.
#[derive(Clone, Debug)]
pub struct Context {
    k: u8,
}

impl Context {
    pub const fn new() -> Self {
        Self { k: 0 }
    }

    fn entry(&self) -> Entry {
        TABLE[self.k as usize]
    }

    fn mps(&self) -> bool {
        self.k & 1 != 0
    }
}

pub mod dec {
    use super::{Context, Entry};

    #[derive(Clone, Debug)]
    struct State {
        a: u32,
        c: u32,
        fence: u32,
    }

    impl State {
        fn init(head: u16) -> Self {
            Self {
                a: 0,
                c: head as u32,
                fence: head.min(0x7f_ff) as u32,
            }
        }

        fn update_fence(&mut self) {
            self.fence = self.c.min(0x7f_ff);
        }
    }

    /// Interface required to use a type as a stream of bits for Z&prime; decoding.
    pub trait Input {
        /// Shift `*reg` left by `count`, then fill the `count` least-significant bits with the
        /// next `count` bits of input, such that the “earliest” input occupies the more
        /// significant bits of `*reg`.
        ///
        /// If fewer than `count` input bits remain, the remaining bits should be set to `1`.
        ///
        /// Thus if the remaining input bits are `1`, `0`, `0`, and `*reg` is initially
        /// `0b11100`, calling this method with `count = 6` should result in `*reg =
        /// 0b11100_100_111`.
        ///
        /// This method should not be called with `count > 16`. Implementations are permitted to
        /// panic or produce garbage if `count > 16`, but this must not lead to undefined behavior.
        fn shift_into(&mut self, reg: &mut u32, count: u32);
    }

    #[derive(Clone, Debug)]
    pub struct BytesInput<B> {
        fetched: u32,
        bits_remaining: u32,
        bytes: B,
    }

    impl<B: Iterator<Item = u8> + std::iter::FusedIterator> BytesInput<B> {
        pub fn new(bytes: B) -> Self {
            Self {
                fetched: Default::default(),
                bits_remaining: 0,
                bytes,
            }
        }
    }

    impl<B: Iterator<Item = u8> + std::iter::FusedIterator> Input for BytesInput<B> {
        fn shift_into(&mut self, reg: &mut u32, count: u32) {
            debug_assert!(count <= 16);

            if self.bits_remaining < 16 {
                let b0 = self.bytes.next().unwrap_or(0xff);
                let b1 = self.bytes.next().unwrap_or(0xff);
                self.fetched |= u32::from_be_bytes([0, 0, b0, b1]) << (16 - self.bits_remaining);
                self.bits_remaining += 16;
            }

            let mut temp = self.fetched as u64;
            temp |= (*reg as u64) << 32;
            temp <<= count;
            self.fetched <<= count;
            self.bits_remaining -= count;
            *reg = (temp >> 32) as u32;
        }
    }

    #[derive(Clone, Debug)]
    pub struct Source<I> {
        state: State,
        input: I,
    }

    impl<I: Input> Source<I> {
        pub fn new(mut input: I) -> Self {
            let mut head = 0;
            input.shift_into(&mut head, 16);
            Self {
                state: State::init(head as u16),
                input,
            }
        }

        /// Decode a single decision from this source using the given context.
        pub fn decode_decision(&mut self, context: &mut Context) -> bool {
            let Source { state, input } = self;
            let Entry {
                delta,
                theta,
                mu,
                lambda,
            } = context.entry();
            let mps = context.mps();

            let z = state.a + delta as u32;
            if z <= state.fence {
                state.a = z;
                return mps;
            }

            let d = 0x60_00 + (z + state.a >> 2);
            let z = z.min(d);

            if z > state.c {
                let z = 0x1_00_00 - z;
                state.a += z;
                state.c += z;

                context.k = lambda;

                let shift = (state.a as u16).leading_ones();
                state.a <<= shift;
                state.a &= 0xff_ff;
                input.shift_into(&mut state.c, shift);
                state.c &= 0xff_ff;

                state.update_fence();

                !mps
            } else {
                if state.a >= theta as u32 {
                    context.k = mu;
                }

                state.a = (z << 1) & 0xff_ff;
                input.shift_into(&mut state.c, 1);
                state.c &= 0xff_ff;

                state.update_fence();

                mps
            }
        }

        /// Decode a single decision from this source in passthrough mode, without using a context.
        pub fn decode_decision_passthrough(&mut self) -> bool {
            let Source { state, input } = self;

            let z = 0x80_00 + (state.a >> 1);
            if z > state.c {
                let z = 0x1_00_00 - z;
                state.a += z;
                state.c += z;

                let shift = (state.a as u16).leading_ones();
                state.a <<= shift;
                state.a &= 0xff_ff;
                input.shift_into(&mut state.c, shift);
                state.c &= 0xff_ff;

                state.update_fence();

                true
            } else {
                state.a = (z << 1) & 0xff_ff;
                input.shift_into(&mut state.c, 1);
                state.c &= 0xff_ff;

                state.update_fence();

                false
            }
        }
    }
}

pub use self::dec::{BytesInput, Source};
