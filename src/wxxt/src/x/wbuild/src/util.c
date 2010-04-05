/*   wbuild
     Copyright (C) 1996  Joel N. Weber II <nemo@koa.iolani.honolulu.hi.us>
     
     This program is free software; you can redistribute it and/or
     modify it under the terms of the GNU General Public License
     as published by the Free Software Foundation; either version 2
     of the License, or (at your option) any later version.
     
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.
     
     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/* Introduction. The `wbuild' program reads a file with descriptions
 * of widget classes and generates four files for each class defined
 * there: three C files and a \TeX\ file. The three C files are the two
 * header files and the implementation file for the widget and the \TeX\
 * file contains the documentation for the widget. It should never be
 * necessary to edit the files; they are readable, but only barely so.
 *
 * |MAXSTR| is probably enough for identifiers and such.
 */

#include <config.h>
#include <stdio.h>
#include <string.h>

#include <libit/string.h>
#include <libit/ctype.h>
#include <wbuild.h>
#include <wsym.h>

#if HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <libintl.h>
#define _(String) gettext(String)

#include <libit/malloc.h>

void wbuild_comment(FILE *f)
{
	fprintf(f,
		"/* Generated by wbuild\n"
		" * (generator version %s)\n"
		" */\n", "3.2");
}

/* doctypes */

Doctype doctypes = 0;

void copy_taglist(taglist *dest, taglist *src)
{
	int i;

	for (i = 0; i <= t_privconstraint; i++) {
		(*dest)[i][0] = (*src)[i][0];
		(*dest)[i][1] = (*src)[i][1];
	}
}

void zero_taglist(taglist *dest)
{
	int i;

	for (i = 0; i <= t_privconstraint; i++) {
		(*dest)[i][0] = (*dest)[i][1] = 0;
	}
}


void add_doctype(Doctype d)
{
	d->next = doctypes;
	doctypes = d;
}


static char *escape_string(char *src)
{
	char *dest, *s;

	dest = s = malloc(strlen(src) + 1);
	while (*src) {
		switch (*src) {
			case '\\':
				src++;
				switch (*src) {
					case 'a': *s = '\a'; break;
					case 'b': *s = '\b'; break;
					case 'f': *s = '\f'; break;
					case 'n': *s = '\n'; break;
					case 'r': *s = '\r'; break;
					case 't': *s = '\t'; break;
					case 'v': *s = '\v'; break;
					case 'B': *s = '\\'; break;
					default: *s = *src;
				}
				src++;
				s++;
				break;
			case '\"':
				src++;
				break;
			default:
				*s = *src;
				s++;
				src++;
		}
	}
	*s = 0;
	return dest;
}

void set_doctag(taglist tag, char *name, char *open, char *close)
{
	name = escape_string(name);
	open = escape_string(open);
	close = escape_string(close);
	if (!strcmp("filename", name)) {
		tag[t_filename][0] = strdup(open);
		tag[t_filename][1] = strdup(close);
	}
	if (!strcmp("start", name)) {
		tag[t_start][0] = strdup(open);
		tag[t_start][1] = strdup(close);
	}
	if (!strcmp("class", name)) {
		tag[t_class][0] = strdup(open);
		tag[t_class][1] = strdup(close);
	}
	if (!strcmp("name", name)) {
		tag[t_name][0] = strdup(open);
		tag[t_name][1] = strdup(close);
	}
	if (!strcmp("name2", name)) {
		tag[t_name2][0] = strdup(open);
		tag[t_name2][1] = strdup(close);
	}
	if (!strcmp("superclass", name)) {
		tag[t_superclass][0] = strdup(open);
		tag[t_superclass][1] = strdup(close);
	}
	if (!strcmp("publicvars", name)) {
		tag[t_publicvars][0] = strdup(open);
		tag[t_publicvars][1] = strdup(close);
	}
	if (!strcmp("privatevars", name)) {
		tag[t_privatevars][0] = strdup(open);
		tag[t_privatevars][1] = strdup(close);
	}
	if (!strcmp("actions", name)) {
		tag[t_actions][0] = strdup(open);
		tag[t_actions][1] = strdup(close);
	}
	if (!strcmp("translations", name)) {
		tag[t_translations][0] = strdup(open);
		tag[t_translations][1] = strdup(close);
	}
	if (!strcmp("exports", name)) {
		tag[t_exports][0] = strdup(open);
		tag[t_exports][1] = strdup(close);
	}
	if (!strcmp("methods", name)) {
		tag[t_methods][0] = strdup(open);
		tag[t_methods][1] = strdup(close);
	}
	if (!strcmp("imports", name)) {
		tag[t_imports][0] = strdup(open);
		tag[t_imports][1] = strdup(close);
	}
	if (!strcmp("utilities", name)) {
		tag[t_utilities][0] = strdup(open);
		tag[t_utilities][1] = strdup(close);
	}
	if (!strcmp("classvars", name)) {
		tag[t_classvars][0] = strdup(open);
		tag[t_classvars][1] = strdup(close);
	}
	if (!strcmp("section", name)) {
		tag[t_section][0] = strdup(open);
		tag[t_section][1] = strdup(close);
	}
	if (!strcmp("macro", name)) {
		tag[t_macro][0] = strdup(open);
		tag[t_macro][1] = strdup(close);
	}
	if (!strcmp("publicvar", name)) {
		tag[t_publicvar][0] = strdup(open);
		tag[t_publicvar][1] = strdup(close);
	}
	if (!strcmp("action", name)) {
		tag[t_action][0] = strdup(open);
		tag[t_action][1] = strdup(close);
	}
	if (!strcmp("code", name)) {
		tag[t_code][0] = strdup(open);
		tag[t_code][1] = strdup(close);
	}
	if (!strcmp("table", name)) {
		tag[t_table][0] = strdup(open);
		tag[t_table][1] = strdup(close);
	}
	if (!strcmp("tablehead", name)) {
		tag[t_tablehead][0] = strdup(open);
		tag[t_tablehead][1] = strdup(close);
	}
	if (!strcmp("row", name)) {
		tag[t_row][0] = strdup(open);
		tag[t_row][1] = strdup(close);
	}
	if (!strcmp("resname", name)) {
		tag[t_resname][0] = strdup(open);
		tag[t_resname][1] = strdup(close);
	}
	if (!strcmp("resclass", name)) {
		tag[t_resclass][0] = strdup(open);
		tag[t_resclass][1] = strdup(close);
	}
	if (!strcmp("restype", name)) {
		tag[t_restype][0] = strdup(open);
		tag[t_restype][1] = strdup(close);
	}
	if (!strcmp("resdefault", name)) {
		tag[t_resdefault][0] = strdup(open);
		tag[t_resdefault][1] = strdup(close);
	}
	if (!strcmp("inline", name)) {
		tag[t_inline][0] = strdup(open);
		tag[t_inline][1] = strdup(close);
	}
	if (!strcmp("underline", name)) {
		tag[t_underline][0] = strdup(open);
		tag[t_underline][1] = strdup(close);
	}
	if (!strcmp("backslash", name)) {
		tag[t_backslash][0] = strdup(open);
		tag[t_backslash][1] = strdup(close);
	}
	if (!strcmp("tilde", name)) {
		tag[t_tilde][0] = strdup(open);
		tag[t_tilde][1] = strdup(close);
	}
	if (!strcmp("hashmark", name)) {
		tag[t_hashmark][0] = strdup(open);
		tag[t_hashmark][1] = strdup(close);
	}
	if (!strcmp("dollar", name)) {
		tag[t_dollar][0] = strdup(open);
		tag[t_dollar][1] = strdup(close);
	}
	if (!strcmp("less", name)) {
		tag[t_less][0] = strdup(open);
		tag[t_less][1] = strdup(close);
	}
	if (!strcmp("greater", name)) {
		tag[t_greater][0] = strdup(open);
		tag[t_greater][1] = strdup(close);
	}
	if (!strcmp("percent", name)) {
		tag[t_percent][0] = strdup(open);
		tag[t_percent][1] = strdup(close);
	}
	if (!strcmp("caret", name)) {
		tag[t_caret][0] = strdup(open);
		tag[t_caret][1] = strdup(close);
	}
	if (!strcmp("ampersand", name)) {
		tag[t_ampersand][0] = strdup(open);
		tag[t_ampersand][1] = strdup(close);
	}
	if (!strcmp("lbrace", name)) {
		tag[t_lbrace][0] = strdup(open);
		tag[t_lbrace][1] = strdup(close);
	}
	if (!strcmp("rbrace", name)) {
		tag[t_rbrace][0] = strdup(open);
		tag[t_rbrace][1] = strdup(close);
	}
	if (!strcmp("bar", name)) {
		tag[t_bar][0] = strdup(open);
		tag[t_bar][1] = strdup(close);
	}
	if (!strcmp("at", name)) {
		tag[t_at][0] = strdup(open);
		tag[t_at][1] = strdup(close);
	}
	if (!strcmp("type", name)) {
		tag[t_type][0] = strdup(open);
		tag[t_type][1] = strdup(close);
	}
	if (!strcmp("incl", name)) {
		tag[t_incl][0] = strdup(open);
		tag[t_incl][1] = strdup(close);
	}
	if (!strcmp("constraints", name)) {
		tag[t_constraints][0] = strdup(open);
		tag[t_constraints][1] = strdup(close);
	}
	if (!strcmp("constraint", name)) {
		tag[t_constraint][0] = strdup(open);
		tag[t_constraint][1] = strdup(close);
	}
	if (!strcmp("privconstraints", name)) {
		tag[t_privconstraints][0] = strdup(open);
		tag[t_privconstraints][1] = strdup(close);
	}
	if (!strcmp("privconstraint", name)) {
		tag[t_privconstraint][0] = strdup(open);
		tag[t_privconstraint][1] = strdup(close);
	}
	free(open);
	free(close);
	free(name);
}

/* Classes. There are functions for adding classes to the list,
 * looking up classes by name and finding the highest superclass to
 * define a certain method. A global variable |classes| holds the list of
 * all classes.
 */

Class classes = 0;
int classnodoc = 0, classnocode = 0;
STRING classfilename = 0;

/* The function |add_class| is used by the parser and is therefore
 * exported. It uses a recursive procedure to append the new class at the
 * end of |classes|. It is assumed that |c->next| is already set to |NULL|.
 */

void add_class(c)
	Class c;
{
	c->next = classes;
	classes = c;
}

/* Strings. Strings are allocated dynamically, but a hash table makes
 * sure that equal strings share the same pointer. The hash technique is
 * a simple open hash.
 *
 * The |strdup| and |malloc| standard functions return |NULL| if they
 * could not allocate enough memory. The two routines |Strdup| and
 * |Malloc| call these routines and check for |NULL|. They abort the
 * program with an `out of memory' message.
 *
 * The macro |new| is a convenient way of allocating unnamed variables
 * via the pointer, as in Pascal.
 */

#define new(ptr) ptr = xmalloc(sizeof(*(ptr)))

/* xmalloc puzzles me.  If we're out of memory, why do we expect to
 * have enough to write something to stderr?  Something to argue about
 * with RMS --nemo
 */

void *xmalloc(size)
	size_t size;
{
	void *t;

	t = malloc(size);
	if (t == NULL) {
		/* I deliberately decided NOT to internationalize--see above
		 */
		fprintf(stderr, "out of memory\n");
		exit(1);
	}
	return t;
}

char * xstrdup(s)
	char *s;
{
	char *t;
	if (s) {
		t = xmalloc(strlen(s) + 1);
		strcpy(t, s);
	} else {
		t = xmalloc(1);
		*t = 0;
	}
	return t;
}

/* The hash table is not initialized, instead a scheme with two extra
 * arrays |r1| and |r2| and a variable |nused| is used, which allows the
 * hash table entries to be initialized the first time they are actually
 * used. This saves time at start up. The opaque type |STRING| (defined
 * in `types'), is implemented here as a bucket in the hash table,
 * containing a pointer to a dynamically allocated string, a link counter
 * and a next-pointer. The link counter is used to deallocate the string
 * when it is no longer used.
 */

#define HASHSIZE 8009

static STRING hashtable[HASHSIZE];
static int r2[HASHSIZE], r1[HASHSIZE];
static int nused = 0;

/* The hash function uses a proven technique of shifting ASCII codes,
 * ignoring overflow. We limit the computation to the first twenty
 * characters of the string.
 */

static int hashval(name)
	char* name;
{
	char *p;
	unsigned int h = 0, g, n = 20;

	if (name == NULL)
		return 0;

	for (p = name; n && *p; p++, n--) {
		h = (h << 4) + (*p);
		if ((g = h & 0xf0000000)) {
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}
	return h % HASHSIZE;
}

/* The function |hash| adds a new string to the buffer. It checks if
 * the string is already there, before creating a new copy.
 *
 * The array |r1| holds the indexes of the hashtable entries that are
 * used. There are |nused| such entries, stored in |r1[0]| through
 * |r1[nused-1]|. The array |r2| is used to look up the position in |r1|
 * where an index is stored. If we want to know if |hashtable[n]| is
 * valid, we first look in |r2[n]|. If |r2[n]| contains nonsense (i.e., a
 * number less than zero or greater or the same as |nused|), we know that
 * entry |n| is invalid. If |r2[n]| points to a valid entry in |r1|, but
 * the entry in |r1| doesn't contain |n|, then entry |n| is also invalid.
 * Only if |r1[r2[n]] == n| is entry |n| valid.
 */

#define valid(n) (0 <= r2[n] && r2[n] < nused && r1[r2[n]] == n)

STRING hash(char *s)
{
	int h;
	STRING t;

	if (!s || s[0] == '\0')
		return 0;

	h = hashval(s);
	if (! valid(h)) {
		new(t);
		t->s = xstrdup(s);
		t->linkcount = 1;
		t->next = 0;
		do {
			r2[h] = nused;
			r1[nused] = h;
			nused++;
		} while (0);
		hashtable[h] = t;
		return t;
	}
	for (t = hashtable[h]; t; t = t->next) {
		if (strcmp(t->s, s) == 0) {
			t->linkcount++;
			return t;
		}
	}
	new(t);
	t->s = xstrdup(s);
	t->linkcount = 1;
	t->next = hashtable[h];
	hashtable[h] = t;
	return t;
}

/* |delete| tries to free the space occupied by a string, but only if
 * the |linkcount| indicates that there are no other pointers to the
 * string.
 *
 * The routine could be made more efficient if the |struct _STRING|
 * contained |h| or, even beter, a backpointer. We will have to see how
 * the performance of Tovenaar is.
 *
 * I'm not sure what Bert was saying in the preceeding paragraph.  gcc
 * generally takes much longer than wbuild, some I'm sure the that whatever
 * it is works fine.  Amazing what you fail to learn when all you do
 * with the code is reformat it.  --nemo
 */

void delete(s)
	STRING s;
{
#if 0
#ifdef NDEBUG
	int h; STRING t;
#endif /*NDEBUG*/

	if (!s)
		return;

	s->linkcount--;
	if (s->linkcount > 0)
		return; /* still in use elsewhere */

#ifdef NDEBUG
	h = hashval(s->s);
	if (hashtable[h] == s)
		hashtable[h] = s->next;
	else {
		for (t = hashtable[h]; t->next != s; t = t->next) ;
		t->next = s->next;
	}
	if (s->s)
		free(s->s);
	free(s);
#else /*NDEBUG*/
	if (s->linkcount < 0)
		debug(("linkcount on %s is %d\n", s->s, s->linkcount));
#endif /*NDEBUG*/
#endif
}

/* To convert a |STRING| to a |char *|, the function |get| must be
 * used.
 */

char *get(s)
	STRING s;
{
	if (!s)
		return "";
	else
		return s->s;
}

STRING hdup(s)
	STRING s;
{
	if (!s)
		return 0;
	s->linkcount++;
	return s;
}

/* |catstr| creates a new |STRING| which is a concatenation of all
 * arguments. The argument list must end with a 0.
 */

#if HAVE_STDARG_H
#ifdef __STDC__
STRING catstr(int n,...)
#else /* not __STDC__ */
STRING catstr(n) int n;
#endif /* not __STDC__ */
#else /* not HAVE_STDARG_H */
STRING catstr(va_alist) va_dcl
#endif /* not HAVE_STDARG_H */
{
	va_list ap;
#if !HAVE_STDARG_H
	int n;
#endif /* !HAVE_STDARG_H */
	unsigned int len = 0;
	char *s, *s1;
	STRING t;
	int i;

#ifdef HAVE_STDARG_H
	va_start(ap, n);
#else /* !HAVE_STDARG_H */
	va_start(ap);
	n = va_arg(ap, int);
#endif /* !HAVE_STDARG_H */
	if (n == 0) {
		va_end(ap);
		return 0;
	} else {
		for (i = n; i > 0; i--) {
			s1 = va_arg(ap, char *);
			len += s1 ? strlen(s1) : 0;
		}
		va_end(ap);
		s = xmalloc((len + 2) * sizeof(char));
		s[0] = '\0';
#if HAVE_STDARG_H
		va_start(ap, n);
#else /* !HAVE_STDARG_H */
		va_start(ap);
		n = va_arg(ap, int);
#endif /* !HAVE_STDARG_H */
		for (i = n; i > 0; i--) {
			s1 = va_arg(ap, char *);
			if (s1) strcat(s, s1);
		}
		va_end(ap);
		t = hash(s);
		return t;
	}
}

/* |Strlen| is another convenience function for |STRING|s, it is
 * equivalent to |strlen(get(s))|, but it also works if |s| is |NULL|.
 */

size_t Strlen(s)
	STRING s;
{
	return s ? (s->s != NULL ? strlen(s->s) : 0) : 0;
}

/* |get_classname| returns the string |s|, but with the first letter
 * converted to uppercase. If the first letter is `X' the second letter is
 * also converted.
 * If the first letter is a \$, it is omitted.
 */

STRING get_classname(s)
	STRING s;
{
	char *h, *h1;
	STRING t;
	h = h1 = xstrdup(s->s);
	if (*h == '$') h++;
	if ('a' <= h[0] && h[0] <= 'z') h[0] += 'A' - 'a';
	if (h[0] == 'X' && 'a' <= h[1] && h[1] <= 'z') h[1] += 'A' - 'a';
	t = hash(h); free(h1);
	return t;
}

/* |get_instname| returns the string |s|, but with the first letter
 * converted to lowercase. If the first letter is `X' the second letter is
 * also converted.
 * If the first letter is a \$, it is omitted.
 */

STRING get_instname(s)
	STRING s;
{
	char *h, *h1;
	STRING t;
	h = h1 = xstrdup(s->s);
	if (*h == '$') h++;
	if ('A' <= h[0] && h[0] <= 'Z') h[0] += 'a' - 'A';
	if (h[0] == 'x' && 'A' <= h[1] && h[1] <= 'Z') h[1] += 'a' - 'A';
	t = hash(h); free(h1);
	return t;
}

/* The function |get_word| copies the next `word' from |s| to |word|. A
 * word is defined as a C identifier.
 */

STRING get_word(s)
	char *s;
{
	char *word;
	STRING t;
	int i;

	if (!s)
		return 0;
	else {
		word = xmalloc(sizeof(char) * strlen(s) + 1);
		for (i = 0; isalnum(s[i]) || s[i] == '_'; i++)
			word[i] = s[i];
		word[i] = '\0';
		t = hash(word);
		free(word);
		return t;
	}
}

#if 0
STRING get_guard(c)
	Class c;
{
	STRING guard;

	foo bar die c compiler TODO

	if (! get_option(&guard, c, hash("guard")))
		guard = catstr(3, "_", get(c->name), "_H_");
	return guard;
}

STRING get_guardP(c)
	Class c;
{
	STRING guard;
	if (! get_option(&guard, c, hash("guardP")))
		guard = catstr(3, "_", get(c->name), "P_H_");
	return guard;
}

#endif

/* The public header file name is constructed from the class name,
 * unless there is an option that names a different file. The directory
 * |dir| is prefixed to it. The name including `.h' should not exceed 12
 * characters, the name of the class is therefore truncated to 10
 * characters.
 */

#define copy_directory \
	if (dir != NULL && dir[0] != '\0') {				\
		strcpy(s, dir);						\
		if (dir[strlen(dir)-1] != '/')				\
			strcat(s, "/");					\
	}

#if 0
STRING get_headername(char *dir, Class c)
{
	STRING t;
	static STRING file = 0;
	char *s;
	if (!file)
		file = hash("file");
    s = Malloc((dir != NULL ? strlen(dir) : 0) + 16);

    @<Copy directory |dir| to |s|@>@;
    if (! get_option(&t, c, file)) t = c->name;
    (void) strncat(s, get(t), 10);
    (void) strcat(s, ".h");
    t = hash(s); free(s);
    return t;
  }
#endif

/* The private header file name is taken from an option or from the
 * class name, prefixed with the directory |dir| and affixed with
 * |"P.h"|. The total length of the name including `P.h' should not
 * exceed 12 characters.
 */

STRING get_headerPname(char *dir, Class c)
{
	char *s;
	static STRING file = 0;
	STRING t;

	if (!file)
		file = hash("file");

	s = xmalloc((dir != NULL ? strlen(dir) : 0) + 16);
	if (dir != NULL && dir[0] != '\0') {
		strcpy(s, dir);
		if (dir[strlen(dir)-1] != '/')
			strcat(s, "/");
	}
	if (c->filenamepart)
		t = c->filenamepart;
	else
		t = c->name;
	/* strncat(s, get(t), 9); */
	strcat(s, get(t));
	strcat(s, "P.h");
	t = hash(s);
	free(s);
	return t;
}

/* The name of the implementation file is normally the same as the name
 * of the class (truncated to 12 characters). But if the option `@@file'
 * is present, the value of that option will be the base name of all C
 * files.
 */

#if 0
  STRING get_implementationname(dir, c)
    char *dir;
    Class c;
  {
    char *s;
    static STRING file = NIL;
    STRING t;
    if (file == NIL) file = hash("file");
    s = Malloc((dir != NULL ? strlen(dir) : 0) + 16);
    @<Copy directory |dir| to |s|@>@;
    if (! get_option(&t, c, file)) t = c->name;
    (void) strncat(s, get(t), 10);
    (void) strcat(s, ".c");
    t = hash(s); free(s);
    return t;
  }
#endif

/* The name of the \TeX-file is normally the same as class name plus
 * `.doc', but the `@@file' option overrides that.
 */

#if 0
  STRING get_docfilename(dir, c)
    char *dir;
    Class c;
  {
    char *s;
    static STRING file = NIL;
    STRING t;
    if (file == NIL) file = hash("file");
    s = Malloc((dir != NULL ? strlen(dir) : 0) + 16);
    @<Copy directory |dir| to |s|@>@;
    if (! get_option(&t, c, file)) t = c->name;
    (void) strncat(s, get(t), 8);
    (void) strcat(s, ".doc");
    t = hash(s); free(s);
    return t;
  }
#endif

/* Declaration functions.
 *
 * |find_class| searches through the list of classes for the one named
 * |name|. It returns |NULL| if no class is found.
 */

static Class find_class(STRING name)
{
	Class h;
	h = classes;
	while (h && h->name != name)
		h = h->next;
	return h;
}

/* |find_superclass| searches through the superclasses of |c| for the
 * class named |name|. It returns |NULL| if no class is found.
 */

Class find_superclass(Class c, STRING name)
{
	if (!c)
		return 0;
	else if (c->name == name)
		return c;
	else
		return find_superclass(c->super, name);
}

/* |find_classvar_class| searches |c| and its superclasses for the
 * first declaration of the identifier |name|. It returns the defining
 * class or |NULL|. |find_instvar_class| is similar, but searches for
 * instance variables (either public or private). |find_constr_class|
 * searches for constraint resources. |find_method_class| searches for
 * methods.
 */

Class find_classvar_class(Class c, STRING name)
{
	Class h;
	Section d;
	if (!c)
		return NULL;
	if ((h = find_classvar_class(c->super, name)))
		return h;
	for (d = c->classvars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == name)
			return c;
	return 0;
}

Class find_instvar_class(Class c, STRING name)
{
	Class h;
	Section d;

	if (!c)
		return 0;
	if ((h = find_instvar_class(c->super, name)))
		return h;
	for (d = c->publicvars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == name)
			return c;
	for (d = c->privatevars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == name)
			return c;
	return 0;
}

Class find_constr_class(Class c, STRING name)
{
	Class h;
	Section d;

	if (!c)
		return 0;
	if ((h = find_constr_class(c->super, name)))
		return h;
	for (d = c->constraints; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == name)
			return c;
	for (d = c->privconstr; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == name)
			return c;
	return 0;
}

Class find_method_class(Class c, STRING name)
{
	Class h;
	Section d;

	if (!c)
		return 0;
	if ((h = find_method_class(c->super, name)))
		return h;
	for (d = c->methods; d; d = d->next)
		if (d->decl && d->decl->tp == Proc && d->decl->name == name)
			return c;
	return 0;
}

/* |find_method| finds the declaration of given method |m| in class
 * |c| or one of its superclasses.
 */

Section find_method(Class c, STRING m)
{
	Section d, h;

	if (!c)
		return 0;
	if ((h = find_method(c->super, m)))
		return h;
	for (d = c->methods; d; d = d->next)
		if (d->decl && d->decl->tp == Proc && d->decl->name == m)
			return d;
	return 0;
}

/* |has_method| checks if class |c| has (re)defined method |m|. This is
 * used by |init_class_parts| in the module `generate', to check if a
 * method should be inherited.
 */

int has_method(Class c, STRING m)
{
	Section d;
	for (d = c->methods; d; d = d->next)
		if (d->decl && d->decl->tp == Proc && d->decl->name == m)
			return 1;
	return 0;
}

/* |has_classvar| checks if class |c| has (re)defined class varaible
 * |m|. This is used by |init_class_parts| in the module `generate'.
 */

int has_classvar(Class c, STRING m)
{
	Section d;
	for (d = c->classvars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == m)
			return 1;
	return 0;
}

/* |find_pubvar| finds the declaration of given public variable |m| in class
 * |c| or one of its superclasses.
 */

Section find_pubvar(Class c, STRING m)
{
	Section d, h;

	if (!c)
		return 0;
	if ((h = find_pubvar(c->super, m)))
		return h;
	for (d = c->publicvars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == m)
			return d;
	return 0;
}

/* |find_pubvar_back| finds the declaration of given public variable
 * |m| in class |c| or one of its superclasses.
 */

Section find_pubvar_back(Class c, STRING m)
{
	Section d, h;

	if (!c)
		return 0;
	for (d = c->publicvars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == m)
			return d;
	if ((h = find_pubvar(c->super, m)))
		return h;
	return 0;
}

/* |find_constraint| finds the declaration of given constraint variable
 * |m| in class |c| or one of its superclasses.
 */

Section find_constraint(Class c, STRING m)
{
	Section d, h;

	if (!c)
		return 0;
	if ((h = find_constraint(c->super, m)))
		return h;
	for (d = c->constraints; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == m)
			return d;
	for (d = c->privconstr; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == m)
			return d;
	return 0;
}

/* |find_classvar_value| looks for the initialization value of a class
 * variable in the class |c| or its superclasses.
 */

STRING find_classvar_value(Class c, STRING name)
{
	Section d;

	for (d = c->classvars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == name)
			return d->decl->value;
	if (c->super)
		return find_classvar_value(c->super, name);
	else
		return 0;
}

/* |find_value| finds the value of an instance variable, by searching
 * back through the hierarchy of classes for the inherited initial value.
 */

STRING find_instvar_value(Class c, STRING name)
{
	Section d;

	for (d = c->publicvars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == name)
			return d->decl->value;
	for (d = c->privatevars; d; d = d->next)
		if (d->decl && d->decl->tp == Var && d->decl->name == name)
			return d->decl->value; /* should be NULL, though */
	if (c->super)
		return find_instvar_value(c->super, name);
	else
		return NULL;
}

/* Hierarchy. The classes are arranged in a tree with the function
 * |set_hierarchy|. This function should be called after all files have
 * been parsed, but before any of the other class handling function above
 * is called.
 *
 * |toplevel| holds the list of classes that have no superclass. The list
 * is chained on the |sister| field.
 */

Class toplevel = NULL;

/* The superclass' name may include one or more slashes, e.g.,
 * |"Xfwf/XfwfLabel"|, meaning that the header files can be found in the
 * |Xfwf| directory, instead of in the directory given by the |-p|
 * command line option.
 *
 * After every class has been given a pointer to its superclass, a check
 * is made to see if there are no cycles: eventually, every chain of
 * |super| must end with |NULL|.
 *
 * The function returns the number of errors.
 */


int set_hierarchy(void)
{
	Class c, c1;
	char *p;
	int err = 0;

	for (c = classes; c; c = c->next) {
		if ((p = strrchr(get(c->superclass), '/')))
			c->super = find_class(hash(p + 1));
		else
			c->super = find_class(c->superclass);
		if (c->super == NULL) {
			c->sister = toplevel;
			toplevel = c;
		} else {
			c->sister = c->super->daughters;
			c->super->daughters = c;
		}
	}
	for (c = classes; c; c = c->next) {
		for (c1 = c->super; c1; c1 = c1->super) {
			if (c1 == c) {
				fprintf(stderr,
					_("%s:%d: cyclic dependency found "
					"in superclasses of %s\n"),
					get(c->filename), c->lineno,
					get(c->name));
				err++;
				break;
			}
		}
	}
	return err;
}

/* strncmp replacement.
 */

int strneq(char *a, char *b, int n)
{
	if (!a)
		return (!b) || (*b == '\0');
	if (!b)
		return 0;
	while (n && *a && (*a == *b)) {
		a++;
		b++;
		n--;
	}
	return n == 0 || *a == *b /* == '\0' */;
}