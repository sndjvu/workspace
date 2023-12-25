#ifndef SNDJVU_H
#define SNDJVU_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct sndjvu_doc sndjvu_doc;

long long
sndjvu_startdoc(const void *buf, unsigned long long len, sndjvu_doc **out);

void
sndjvu_untiedoc(sndjvu_doc *doc);

void
sndjvu_closedoc(sndjvu_doc *doc);

enum {
	SNDJVU_SINGLEPAGE = 1,
	SNDJVU_MULTIPAGE
};

int
sndjvu_dockind(sndjvu_doc *doc);

typedef struct sndjvu_page sndjvu_page;
typedef struct sndjvu_multipage sndjvu_multipage;

struct sndjvu_info {
	unsigned short width;
	unsigned short height;
	unsigned char major_version;
	unsigned char minor_version;
	unsigned short dpi;
	unsigned char gamma;
	unsigned char flags;
};

int
sndjvu_pageinfo(sndjvu_page *page, struct sndjvu_info *out);

typedef unsigned long long sndjvu_pos;

int
sndjvu_isend(sndjvu_pos pos);

typedef struct sndjvu_elem *sndjvu_elem;

void
sndjvu_untieelem(sndjvu_elem *elem);

void
sndjvu_closeelem(sndjvu_elem *elem);

sndjvu_pos
sndjvu_nextelem(sndjvu_elem *elem);

sndjvu_pos
sndjvu_pageelems(sndjvu_page *page);

long long
sndjvu_parseelem(sndjvu_pos pos, const void *buf, unsigned long long len, sndjvu_elem **out);

enum {
	SNDJVU_ANTA = 1,
	SNDJVU_ANTZ,
	SNDJVU_TXTA,
	SNDJVU_TXTZ,
	SNDJVU_DJBZ,
	SNDJVU_SJBZ,
	SNDJVU_FG44,
	SNDJVU_BG44,
	SNDJVU_FGBZ,
	SNDJVU_INCL,
	SNDJVU_BGJP,
	SNDJVU_FGJP,
	SNDJVU_SMMR
};

int
sndjvu_elemkind(sndjvu_elem *elem);

void *
sndjvu_elemdata(sndjvu_elem *elem);

unsigned long
sndjvu_elemlen(sndjvu_elem *elem);

typedef struct sndjvu_ant *sndjvu_ant;

sndjvu_ant *
sndjvu_startant(const void *buf, unsigned long long len);

int
sndjvu_nextant(sndjvu_ant *ant);

void
sndjvu_closeant(sndjvu_ant *ant);

enum {
	SNDJVU_ANTBACKGROUND = 1,
	SNDJVU_ANTZOOM,
	SNDJVU_ANTMODE,
	SNDJVU_ANTALIGN,
	SNDJVU_ANTMAPAREA,
	SNDJVU_ANTMETADATA,
	SNDJVU_ANTXMP
}

int
sndjvu_antkind(sndjvu_ant *ant);

char *
sndjvu_antdata(sndjvu_ant *ant);

void
sndjvu_unquote(char *quoted, char **data_out, unsigned **len_out);

long
sndjvu_antbackground(sndjvu_ant *ant);

enum {
	SNDJVU_ZOOMSTRETCH = 10000,
	SNDJVU_ZOOMONE2ONE,
	SNDJVU_ZOOMWIDTH,
	SNDJVU_ZOOMPAGE,
};

int
sndjvu_antzoom(sndjvu_ant *ant);

enum {
	SNDJVU_MODECOLOR = 1,
	SNDJVU_MODEBW,
	SNDJVU_MODEFORE,
	SNDJVU_MODEBLACK
};

int
sndjvu_antmode(sndjvu_ant *ant);

enum {
	SNDJVU_ALIGNLEFT = 1,
	SNDJVU_ALIGNCENTERH = 2,
	SNDJVU_ALIGNRIGHT = 3,
	SNDJVU_ALIGNMASKH = 3,
	SNDJVU_ALIGNTOP = 1<<2,
	SNDJVU_ALIGNCENTERV = 2<<2,
	SNDJVU_ALIGNBOTTOM = 3<<2,
	SNDJVU_ALIGNMASKV = 3<<2,
};

int
sndjvu_antalign(sndjvu_ant *ant);

typedef struct sndjvu_maparea sndjvu_maparea;

char *
sndjvu_mapareaurl(sndjvu_maparea *map);

char *
sndjvu_mapareatarget(sndjvu_maparea *map);

char *
sndjvu_mapareacomment(sndjvu_maparea *map);

enum {
	SNDJVU_SHAPERECT,
	SNDJVU_SHAPEOVAL,
	SNDJVU_SHAPETEXT,
	SNDJVU_SHAPEPOLY,
	SNDJVU_SHAPELINE
}

int
sndjvu_mapareashape(sndjvu_maparea *map);

struct sndjvu_point {
	unsigned long x;
	unsigned long y;
};

struct sndjvu_box {
	struct sndjvu_point origin;
	unsigned long width;
	unsigned long height;
};

int
sndjvu_shapebox(sndjvu_maparea *map, struct sndjvu_box *out);

int
sndjvu_borderavis(sndjvu_maparea *map);

long long
sndjvu_shapecolors(sndjvu_maparea *map);

long long
sndjvu_textcolors(sndjvu_maparea *map);

int
sndjvu_textpushpin(sndjvu_maparea *map);

struct sndjvu_point *
sndjvu_shapevertices(sndjvu_maparea *map);

int
sndjvu_shapenumvertices(sndjvu_maparea *map);

int
sndjvu_linearrow(sndjvu_maparea *map);

int
sndjvu_linewidth(sndjvu_maparea *map);

enum {
	SNDJVU_BORDERNONE,
	SNDJVU_BORDERXOR,
	SNDJVU_BORDERCOLOR,
	SNDJVU_BORDERSHADOWIN,
	SNDJVU_BORDERSHADOWOUT,
	SNDJVU_BORDERSHADOWEIN,
	SNDJVU_BORDERSHADOWEOUT,
};

int
sndjvu_borderkind(sndjvu_maparea *map);

long
sndjvu_bordercolor(sndjvu_maparea *map);

int
sndjvu_bordershadow(sndjvu_maparea *map);

struct sndjvu_metadatakv {
	char *key;
	char *value;
};

struct sndjvu_metadatakv *
sndjvu_antmetadata(sndjvu_ant *ant);

int
sndjvu_antmetadatalen(sndjvu_ant *ant);

struct sndjvu_zone {
	unsigned char data[17];
}

enum {
	SNDJVU_ZONEPAGE = 1,
	SNDJVU_ZONECOLUMN,
	SNDJVU_ZONEREGION,
	SNDJVU_ZONEPARAGRAPH,
	SNDJVU_ZONELINE,
	SNDJVU_ZONEWORD,
	SNDJVU_ZONECHARACTER
};

int
sndjvu_zonekind(const struct sndjvu_zone *zone);

short
sndjvu_zonexoffset(const struct sndjvu_zone *zone);

short
sndjvu_zoneyoffset(const struct sndjvu_zone *zone);

short
sndjvu_zonewidth(const struct sndjvu_zone *zone);

short
sndjvu_zoneheight(const struct sndjvu_zone *zone);

unsigned long
sndjvu_zonetextlen(const struct sndjvu_zone *zone);

unsigned long
sndjvu_zonenumchildren(const struct sndjvu_zone *zone);

struct sndjvu_txt {
	char *text;
	unsigned long text_len;
	struct sndjvu_zone *zones;
	unsigned long zones_len;
};

int
sndjvu_parsetxt(const void *buf, unsigned long long len, struct sndjvu_txt *out);

sndjvu_pos
sndjvu_multipagecomponents(sndjvu_multipage *multipage);

typedef struct sndjvu_component sndjvu_component;

long long
sndjvu_parsecomponent(sndjvu_pos pos, const void *buf, unsigned long long len, sndjvu_component **out);

enum {
	SNDJVU_DJVI,
	SNDJVU_DJVU,
	SNDJVU_THUM
};

int
sndjvu_componentkind(sndjvu_component *component);

sndjvu_pos
sndjvu_componentbody(sndjvu_component *component);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SNDJVU_H */
