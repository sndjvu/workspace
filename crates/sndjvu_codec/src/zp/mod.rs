//! Implements the Z&prime;-Coder, a binary arithmetic coder.
//!
//! Much of the code for this module is adapted from the source files `libdjvu/ZPCodec.h` and
//! `libdjvu/ZPCodec.cpp` included in DjVuLibre version 3.5.28.

#[derive(Clone, Copy, Debug)]
#[allow(non_snake_case)]
struct Entry {
    Δ: u16,
    θ: u16,
    μ: u8,
    λ: u8,
}

#[rustfmt::skip]
static TABLE: [Entry; 251] = [
    Entry { Δ: 0x8000, θ: 0x0000, μ:  84, λ: 145 },
    Entry { Δ: 0x8000, θ: 0x0000, μ:   3, λ:   4 },
    Entry { Δ: 0x8000, θ: 0x0000, μ:   4, λ:   3 },
    Entry { Δ: 0x6bbd, θ: 0x10a5, μ:   5, λ:   1 },
    Entry { Δ: 0x6bbd, θ: 0x10a5, μ:   6, λ:   2 },
    Entry { Δ: 0x5d45, θ: 0x1f28, μ:   7, λ:   3 },
    Entry { Δ: 0x5d45, θ: 0x1f28, μ:   8, λ:   4 },
    Entry { Δ: 0x51b9, θ: 0x2bd3, μ:   9, λ:   5 },
    Entry { Δ: 0x51b9, θ: 0x2bd3, μ:  10, λ:   6 },
    Entry { Δ: 0x4813, θ: 0x36e3, μ:  11, λ:   7 },
    Entry { Δ: 0x4813, θ: 0x36e3, μ:  12, λ:   8 },
    Entry { Δ: 0x3fd5, θ: 0x408c, μ:  13, λ:   9 },
    Entry { Δ: 0x3fd5, θ: 0x408c, μ:  14, λ:  10 },
    Entry { Δ: 0x38b1, θ: 0x48fd, μ:  15, λ:  11 },
    Entry { Δ: 0x38b1, θ: 0x48fd, μ:  16, λ:  12 },
    Entry { Δ: 0x3275, θ: 0x505d, μ:  17, λ:  13 },
    Entry { Δ: 0x3275, θ: 0x505d, μ:  18, λ:  14 },
    Entry { Δ: 0x2cfd, θ: 0x56d0, μ:  19, λ:  15 },
    Entry { Δ: 0x2cfd, θ: 0x56d0, μ:  20, λ:  16 },
    Entry { Δ: 0x2825, θ: 0x5c71, μ:  21, λ:  17 },
    Entry { Δ: 0x2825, θ: 0x5c71, μ:  22, λ:  18 },
    Entry { Δ: 0x23ab, θ: 0x615b, μ:  23, λ:  19 },
    Entry { Δ: 0x23ab, θ: 0x615b, μ:  24, λ:  20 },
    Entry { Δ: 0x1f87, θ: 0x65a5, μ:  25, λ:  21 },
    Entry { Δ: 0x1f87, θ: 0x65a5, μ:  26, λ:  22 },
    Entry { Δ: 0x1bbb, θ: 0x6962, μ:  27, λ:  23 },
    Entry { Δ: 0x1bbb, θ: 0x6962, μ:  28, λ:  24 },
    Entry { Δ: 0x1845, θ: 0x6ca2, μ:  29, λ:  25 },
    Entry { Δ: 0x1845, θ: 0x6ca2, μ:  30, λ:  26 },
    Entry { Δ: 0x1523, θ: 0x6f74, μ:  31, λ:  27 },
    Entry { Δ: 0x1523, θ: 0x6f74, μ:  32, λ:  28 },
    Entry { Δ: 0x1253, θ: 0x71e6, μ:  33, λ:  29 },
    Entry { Δ: 0x1253, θ: 0x71e6, μ:  34, λ:  30 },
    Entry { Δ: 0x0fcf, θ: 0x7404, μ:  35, λ:  31 },
    Entry { Δ: 0x0fcf, θ: 0x7404, μ:  36, λ:  32 },
    Entry { Δ: 0x0d95, θ: 0x75d6, μ:  37, λ:  33 },
    Entry { Δ: 0x0d95, θ: 0x75d6, μ:  38, λ:  34 },
    Entry { Δ: 0x0b9d, θ: 0x7768, μ:  39, λ:  35 },
    Entry { Δ: 0x0b9d, θ: 0x7768, μ:  40, λ:  36 },
    Entry { Δ: 0x09e3, θ: 0x78c2, μ:  41, λ:  37 },
    Entry { Δ: 0x09e3, θ: 0x78c2, μ:  42, λ:  38 },
    Entry { Δ: 0x0861, θ: 0x79ea, μ:  43, λ:  39 },
    Entry { Δ: 0x0861, θ: 0x79ea, μ:  44, λ:  40 },
    Entry { Δ: 0x0711, θ: 0x7ae7, μ:  45, λ:  41 },
    Entry { Δ: 0x0711, θ: 0x7ae7, μ:  46, λ:  42 },
    Entry { Δ: 0x05f1, θ: 0x7bbe, μ:  47, λ:  43 },
    Entry { Δ: 0x05f1, θ: 0x7bbe, μ:  48, λ:  44 },
    Entry { Δ: 0x04f9, θ: 0x7c75, μ:  49, λ:  45 },
    Entry { Δ: 0x04f9, θ: 0x7c75, μ:  50, λ:  46 },
    Entry { Δ: 0x0425, θ: 0x7d0f, μ:  51, λ:  47 },
    Entry { Δ: 0x0425, θ: 0x7d0f, μ:  52, λ:  48 },
    Entry { Δ: 0x0371, θ: 0x7d91, μ:  53, λ:  49 },
    Entry { Δ: 0x0371, θ: 0x7d91, μ:  54, λ:  50 },
    Entry { Δ: 0x02d9, θ: 0x7dfe, μ:  55, λ:  51 },
    Entry { Δ: 0x02d9, θ: 0x7dfe, μ:  56, λ:  52 },
    Entry { Δ: 0x0259, θ: 0x7e5a, μ:  57, λ:  53 },
    Entry { Δ: 0x0259, θ: 0x7e5a, μ:  58, λ:  54 },
    Entry { Δ: 0x01ed, θ: 0x7ea6, μ:  59, λ:  55 },
    Entry { Δ: 0x01ed, θ: 0x7ea6, μ:  60, λ:  56 },
    Entry { Δ: 0x0193, θ: 0x7ee6, μ:  61, λ:  57 },
    Entry { Δ: 0x0193, θ: 0x7ee6, μ:  62, λ:  58 },
    Entry { Δ: 0x0149, θ: 0x7f1a, μ:  63, λ:  59 },
    Entry { Δ: 0x0149, θ: 0x7f1a, μ:  64, λ:  60 },
    Entry { Δ: 0x010b, θ: 0x7f45, μ:  65, λ:  61 },
    Entry { Δ: 0x010b, θ: 0x7f45, μ:  66, λ:  62 },
    Entry { Δ: 0x00d5, θ: 0x7f6b, μ:  67, λ:  63 },
    Entry { Δ: 0x00d5, θ: 0x7f6b, μ:  68, λ:  64 },
    Entry { Δ: 0x00a5, θ: 0x7f8d, μ:  69, λ:  65 },
    Entry { Δ: 0x00a5, θ: 0x7f8d, μ:  70, λ:  66 },
    Entry { Δ: 0x007b, θ: 0x7faa, μ:  71, λ:  67 },
    Entry { Δ: 0x007b, θ: 0x7faa, μ:  72, λ:  68 },
    Entry { Δ: 0x0057, θ: 0x7fc3, μ:  73, λ:  69 },
    Entry { Δ: 0x0057, θ: 0x7fc3, μ:  74, λ:  70 },
    Entry { Δ: 0x003b, θ: 0x7fd7, μ:  75, λ:  71 },
    Entry { Δ: 0x003b, θ: 0x7fd7, μ:  76, λ:  72 },
    Entry { Δ: 0x0023, θ: 0x7fe7, μ:  77, λ:  73 },
    Entry { Δ: 0x0023, θ: 0x7fe7, μ:  78, λ:  74 },
    Entry { Δ: 0x0013, θ: 0x7ff2, μ:  79, λ:  75 },
    Entry { Δ: 0x0013, θ: 0x7ff2, μ:  80, λ:  76 },
    Entry { Δ: 0x0007, θ: 0x7ffa, μ:  81, λ:  77 },
    Entry { Δ: 0x0007, θ: 0x7ffa, μ:  82, λ:  78 },
    Entry { Δ: 0x0001, θ: 0x7fff, μ:  81, λ:  79 },
    Entry { Δ: 0x0001, θ: 0x7fff, μ:  82, λ:  80 },
    Entry { Δ: 0x5695, θ: 0x0000, μ:   9, λ:  85 },
    Entry { Δ: 0x24ee, θ: 0x0000, μ:  86, λ: 226 },
    Entry { Δ: 0x8000, θ: 0x0000, μ:   5, λ:   6 },
    Entry { Δ: 0x0d30, θ: 0x0000, μ:  88, λ: 176 },
    Entry { Δ: 0x481a, θ: 0x0000, μ:  89, λ: 143 },
    Entry { Δ: 0x0481, θ: 0x0000, μ:  90, λ: 138 },
    Entry { Δ: 0x3579, θ: 0x0000, μ:  91, λ: 141 },
    Entry { Δ: 0x017a, θ: 0x0000, μ:  92, λ: 112 },
    Entry { Δ: 0x24ef, θ: 0x0000, μ:  93, λ: 135 },
    Entry { Δ: 0x007b, θ: 0x0000, μ:  94, λ: 104 },
    Entry { Δ: 0x1978, θ: 0x0000, μ:  95, λ: 133 },
    Entry { Δ: 0x0028, θ: 0x0000, μ:  96, λ: 100 },
    Entry { Δ: 0x10ca, θ: 0x0000, μ:  97, λ: 129 },
    Entry { Δ: 0x000d, θ: 0x0000, μ:  82, λ:  98 },
    Entry { Δ: 0x0b5d, θ: 0x0000, μ:  99, λ: 127 },
    Entry { Δ: 0x0034, θ: 0x0000, μ:  76, λ:  72 },
    Entry { Δ: 0x078a, θ: 0x0000, μ: 101, λ: 125 },
    Entry { Δ: 0x00a0, θ: 0x0000, μ:  70, λ: 102 },
    Entry { Δ: 0x050f, θ: 0x0000, μ: 103, λ: 123 },
    Entry { Δ: 0x0117, θ: 0x0000, μ:  66, λ:  60 },
    Entry { Δ: 0x0358, θ: 0x0000, μ: 105, λ: 121 },
    Entry { Δ: 0x01ea, θ: 0x0000, μ: 106, λ: 110 },
    Entry { Δ: 0x0234, θ: 0x0000, μ: 107, λ: 119 },
    Entry { Δ: 0x0144, θ: 0x0000, μ:  66, λ: 108 },
    Entry { Δ: 0x0173, θ: 0x0000, μ: 109, λ: 117 },
    Entry { Δ: 0x0234, θ: 0x0000, μ:  60, λ:  54 },
    Entry { Δ: 0x00f5, θ: 0x0000, μ: 111, λ: 115 },
    Entry { Δ: 0x0353, θ: 0x0000, μ:  56, λ:  48 },
    Entry { Δ: 0x00a1, θ: 0x0000, μ:  69, λ: 113 },
    Entry { Δ: 0x05c5, θ: 0x0000, μ: 114, λ: 134 },
    Entry { Δ: 0x011a, θ: 0x0000, μ:  65, λ:  59 },
    Entry { Δ: 0x03cf, θ: 0x0000, μ: 116, λ: 132 },
    Entry { Δ: 0x01aa, θ: 0x0000, μ:  61, λ:  55 },
    Entry { Δ: 0x0285, θ: 0x0000, μ: 118, λ: 130 },
    Entry { Δ: 0x0286, θ: 0x0000, μ:  57, λ:  51 },
    Entry { Δ: 0x01ab, θ: 0x0000, μ: 120, λ: 128 },
    Entry { Δ: 0x03d3, θ: 0x0000, μ:  53, λ:  47 },
    Entry { Δ: 0x011a, θ: 0x0000, μ: 122, λ: 126 },
    Entry { Δ: 0x05c5, θ: 0x0000, μ:  49, λ:  41 },
    Entry { Δ: 0x00ba, θ: 0x0000, μ: 124, λ:  62 },
    Entry { Δ: 0x08ad, θ: 0x0000, μ:  43, λ:  37 },
    Entry { Δ: 0x007a, θ: 0x0000, μ:  72, λ:  66 },
    Entry { Δ: 0x0ccc, θ: 0x0000, μ:  39, λ:  31 },
    Entry { Δ: 0x01eb, θ: 0x0000, μ:  60, λ:  54 },
    Entry { Δ: 0x1302, θ: 0x0000, μ:  33, λ:  25 },
    Entry { Δ: 0x02e6, θ: 0x0000, μ:  56, λ:  50 },
    Entry { Δ: 0x1b81, θ: 0x0000, μ:  29, λ: 131 },
    Entry { Δ: 0x045e, θ: 0x0000, μ:  52, λ:  46 },
    Entry { Δ: 0x24ef, θ: 0x0000, μ:  23, λ:  17 },
    Entry { Δ: 0x0690, θ: 0x0000, μ:  48, λ:  40 },
    Entry { Δ: 0x2865, θ: 0x0000, μ:  23, λ:  15 },
    Entry { Δ: 0x09de, θ: 0x0000, μ:  42, λ: 136 },
    Entry { Δ: 0x3987, θ: 0x0000, μ: 137, λ:   7 },
    Entry { Δ: 0x0dc8, θ: 0x0000, μ:  38, λ:  32 },
    Entry { Δ: 0x2c99, θ: 0x0000, μ:  21, λ: 139 },
    Entry { Δ: 0x10ca, θ: 0x0000, μ: 140, λ: 172 },
    Entry { Δ: 0x3b5f, θ: 0x0000, μ:  15, λ:   9 },
    Entry { Δ: 0x0b5d, θ: 0x0000, μ: 142, λ: 170 },
    Entry { Δ: 0x5695, θ: 0x0000, μ:   9, λ:  85 },
    Entry { Δ: 0x078a, θ: 0x0000, μ: 144, λ: 168 },
    Entry { Δ: 0x8000, θ: 0x0000, μ: 141, λ: 248 },
    Entry { Δ: 0x050f, θ: 0x0000, μ: 146, λ: 166 },
    Entry { Δ: 0x24ee, θ: 0x0000, μ: 147, λ: 247 },
    Entry { Δ: 0x0358, θ: 0x0000, μ: 148, λ: 164 },
    Entry { Δ: 0x0d30, θ: 0x0000, μ: 149, λ: 197 },
    Entry { Δ: 0x0234, θ: 0x0000, μ: 150, λ: 162 },
    Entry { Δ: 0x0481, θ: 0x0000, μ: 151, λ:  95 },
    Entry { Δ: 0x0173, θ: 0x0000, μ: 152, λ: 160 },
    Entry { Δ: 0x017a, θ: 0x0000, μ: 153, λ: 173 },
    Entry { Δ: 0x00f5, θ: 0x0000, μ: 154, λ: 158 },
    Entry { Δ: 0x007b, θ: 0x0000, μ: 155, λ: 165 },
    Entry { Δ: 0x00a1, θ: 0x0000, μ:  70, λ: 156 },
    Entry { Δ: 0x0028, θ: 0x0000, μ: 157, λ: 161 },
    Entry { Δ: 0x011a, θ: 0x0000, μ:  66, λ:  60 },
    Entry { Δ: 0x000d, θ: 0x0000, μ:  81, λ: 159 },
    Entry { Δ: 0x01aa, θ: 0x0000, μ:  62, λ:  56 },
    Entry { Δ: 0x0034, θ: 0x0000, μ:  75, λ:  71 },
    Entry { Δ: 0x0286, θ: 0x0000, μ:  58, λ:  52 },
    Entry { Δ: 0x00a0, θ: 0x0000, μ:  69, λ: 163 },
    Entry { Δ: 0x03d3, θ: 0x0000, μ:  54, λ:  48 },
    Entry { Δ: 0x0117, θ: 0x0000, μ:  65, λ:  59 },
    Entry { Δ: 0x05c5, θ: 0x0000, μ:  50, λ:  42 },
    Entry { Δ: 0x01ea, θ: 0x0000, μ: 167, λ: 171 },
    Entry { Δ: 0x08ad, θ: 0x0000, μ:  44, λ:  38 },
    Entry { Δ: 0x0144, θ: 0x0000, μ:  65, λ: 169 },
    Entry { Δ: 0x0ccc, θ: 0x0000, μ:  40, λ:  32 },
    Entry { Δ: 0x0234, θ: 0x0000, μ:  59, λ:  53 },
    Entry { Δ: 0x1302, θ: 0x0000, μ:  34, λ:  26 },
    Entry { Δ: 0x0353, θ: 0x0000, μ:  55, λ:  47 },
    Entry { Δ: 0x1b81, θ: 0x0000, μ:  30, λ: 174 },
    Entry { Δ: 0x05c5, θ: 0x0000, μ: 175, λ: 193 },
    Entry { Δ: 0x24ef, θ: 0x0000, μ:  24, λ:  18 },
    Entry { Δ: 0x03cf, θ: 0x0000, μ: 177, λ: 191 },
    Entry { Δ: 0x2b74, θ: 0x0000, μ: 178, λ: 222 },
    Entry { Δ: 0x0285, θ: 0x0000, μ: 179, λ: 189 },
    Entry { Δ: 0x201d, θ: 0x0000, μ: 180, λ: 218 },
    Entry { Δ: 0x01ab, θ: 0x0000, μ: 181, λ: 187 },
    Entry { Δ: 0x1715, θ: 0x0000, μ: 182, λ: 216 },
    Entry { Δ: 0x011a, θ: 0x0000, μ: 183, λ: 185 },
    Entry { Δ: 0x0fb7, θ: 0x0000, μ: 184, λ: 214 },
    Entry { Δ: 0x00ba, θ: 0x0000, μ:  69, λ:  61 },
    Entry { Δ: 0x0a67, θ: 0x0000, μ: 186, λ: 212 },
    Entry { Δ: 0x01eb, θ: 0x0000, μ:  59, λ:  53 },
    Entry { Δ: 0x06e7, θ: 0x0000, μ: 188, λ: 210 },
    Entry { Δ: 0x02e6, θ: 0x0000, μ:  55, λ:  49 },
    Entry { Δ: 0x0496, θ: 0x0000, μ: 190, λ: 208 },
    Entry { Δ: 0x045e, θ: 0x0000, μ:  51, λ:  45 },
    Entry { Δ: 0x030d, θ: 0x0000, μ: 192, λ: 206 },
    Entry { Δ: 0x0690, θ: 0x0000, μ:  47, λ:  39 },
    Entry { Δ: 0x0206, θ: 0x0000, μ: 194, λ: 204 },
    Entry { Δ: 0x09de, θ: 0x0000, μ:  41, λ: 195 },
    Entry { Δ: 0x0155, θ: 0x0000, μ: 196, λ: 202 },
    Entry { Δ: 0x0dc8, θ: 0x0000, μ:  37, λ:  31 },
    Entry { Δ: 0x00e1, θ: 0x0000, μ: 198, λ: 200 },
    Entry { Δ: 0x2b74, θ: 0x0000, μ: 199, λ: 243 },
    Entry { Δ: 0x0094, θ: 0x0000, μ:  72, λ:  64 },
    Entry { Δ: 0x201d, θ: 0x0000, μ: 201, λ: 239 },
    Entry { Δ: 0x0188, θ: 0x0000, μ:  62, λ:  56 },
    Entry { Δ: 0x1715, θ: 0x0000, μ: 203, λ: 237 },
    Entry { Δ: 0x0252, θ: 0x0000, μ:  58, λ:  52 },
    Entry { Δ: 0x0fb7, θ: 0x0000, μ: 205, λ: 235 },
    Entry { Δ: 0x0383, θ: 0x0000, μ:  54, λ:  48 },
    Entry { Δ: 0x0a67, θ: 0x0000, μ: 207, λ: 233 },
    Entry { Δ: 0x0547, θ: 0x0000, μ:  50, λ:  44 },
    Entry { Δ: 0x06e7, θ: 0x0000, μ: 209, λ: 231 },
    Entry { Δ: 0x07e2, θ: 0x0000, μ:  46, λ:  38 },
    Entry { Δ: 0x0496, θ: 0x0000, μ: 211, λ: 229 },
    Entry { Δ: 0x0bc0, θ: 0x0000, μ:  40, λ:  34 },
    Entry { Δ: 0x030d, θ: 0x0000, μ: 213, λ: 227 },
    Entry { Δ: 0x1178, θ: 0x0000, μ:  36, λ:  28 },
    Entry { Δ: 0x0206, θ: 0x0000, μ: 215, λ: 225 },
    Entry { Δ: 0x19da, θ: 0x0000, μ:  30, λ:  22 },
    Entry { Δ: 0x0155, θ: 0x0000, μ: 217, λ: 223 },
    Entry { Δ: 0x24ef, θ: 0x0000, μ:  26, λ:  16 },
    Entry { Δ: 0x00e1, θ: 0x0000, μ: 219, λ: 221 },
    Entry { Δ: 0x320e, θ: 0x0000, μ:  20, λ: 220 },
    Entry { Δ: 0x0094, θ: 0x0000, μ:  71, λ:  63 },
    Entry { Δ: 0x432a, θ: 0x0000, μ:  14, λ:   8 },
    Entry { Δ: 0x0188, θ: 0x0000, μ:  61, λ:  55 },
    Entry { Δ: 0x447d, θ: 0x0000, μ:  14, λ: 224 },
    Entry { Δ: 0x0252, θ: 0x0000, μ:  57, λ:  51 },
    Entry { Δ: 0x5ece, θ: 0x0000, μ:   8, λ:   2 },
    Entry { Δ: 0x0383, θ: 0x0000, μ:  53, λ:  47 },
    Entry { Δ: 0x8000, θ: 0x0000, μ: 228, λ:  87 },
    Entry { Δ: 0x0547, θ: 0x0000, μ:  49, λ:  43 },
    Entry { Δ: 0x481a, θ: 0x0000, μ: 230, λ: 246 },
    Entry { Δ: 0x07e2, θ: 0x0000, μ:  45, λ:  37 },
    Entry { Δ: 0x3579, θ: 0x0000, μ: 232, λ: 244 },
    Entry { Δ: 0x0bc0, θ: 0x0000, μ:  39, λ:  33 },
    Entry { Δ: 0x24ef, θ: 0x0000, μ: 234, λ: 238 },
    Entry { Δ: 0x1178, θ: 0x0000, μ:  35, λ:  27 },
    Entry { Δ: 0x1978, θ: 0x0000, μ: 138, λ: 236 },
    Entry { Δ: 0x19da, θ: 0x0000, μ:  29, λ:  21 },
    Entry { Δ: 0x2865, θ: 0x0000, μ:  24, λ:  16 },
    Entry { Δ: 0x24ef, θ: 0x0000, μ:  25, λ:  15 },
    Entry { Δ: 0x3987, θ: 0x0000, μ: 240, λ:   8 },
    Entry { Δ: 0x320e, θ: 0x0000, μ:  19, λ: 241 },
    Entry { Δ: 0x2c99, θ: 0x0000, μ:  22, λ: 242 },
    Entry { Δ: 0x432a, θ: 0x0000, μ:  13, λ:   7 },
    Entry { Δ: 0x3b5f, θ: 0x0000, μ:  16, λ:  10 },
    Entry { Δ: 0x447d, θ: 0x0000, μ:  13, λ: 245 },
    Entry { Δ: 0x5695, θ: 0x0000, μ:  10, λ:   2 },
    Entry { Δ: 0x5ece, θ: 0x0000, μ:   7, λ:   1 },
    Entry { Δ: 0x8000, θ: 0x0000, μ: 244, λ:  83 },
    Entry { Δ: 0x8000, θ: 0x0000, μ: 249, λ: 250 },
    Entry { Δ: 0x5695, θ: 0x0000, μ:  10, λ:   2 },
    Entry { Δ: 0x481a, θ: 0x0000, μ:  89, λ: 143 },
    Entry { Δ: 0x481a, θ: 0x0000, μ: 230, λ: 246 },
];

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

pub mod dec;
pub use dec::Decoder;

pub mod enc;
pub use enc::Encoder;