/*!
 * @file su/xml/render.h
 * @brief Render XML documents to a `FILE *`
 *
 * This library provides a C interface for opening and closing
 * XML tags as well as filling them with content. It is mainly
 * intended for constructing XML/HTML documents by directly
 * writing them to a `FILE *`.
 *
 * It's main advantage over plain `printf()` is that it keeps
 * track of open tags, enabling it to automatically close
 * open tags (saving a few lines of code) using `su_xml_close_all()`
 * and `su_xml_close_including()` and/or to detect errors in the
 * programmer's XML nesting. For information on its sanity
 * checking abilities see the documentation of `su_xml_close_tag()`.
 *
 * Currently it has some limitations (possibly incomplete list):
 *
 * * It does not give the calling code feedback if errors occurred
 * * It doesn't do validity checking of tags
 * * It can't generate pretty output (i. e. properly indented)
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

//! @brief Internal linked list type
typedef struct su_xml_stack su_xml_stack_t;

/*!
 * @brief State and configuration of xml generation.
 *
 * Struct containing both state and configuration of this module.
 * See `su_xml_init()` for usage instructions.
 *
 * @see su_xml_init
 * @see su_xml_free
 */
typedef struct su_xml_context {
    su_xml_stack_t *stack;             //!< linked list used internally to keep track of open tags
    FILE           *out;               //!< Where to write output, defaults to stdout
    bool            debug;             //!< whether to print extra information about detected errors to stderr.
    bool            closing_slash;     //!< whether to output a closing slash at the end of an empty tag
} su_xml_context_t;

/*!
 * @brief Initialize the `su_xml_context_t` structure.
 *
 * Initialize a `su_xml_context_t` with default values:
 *
 * * output to `stdout`
 * * no warnings
 * * closing slashes enabled
 *
 * This function should always be called first. If you want
 * to use different settings than the default ones, update
 * the struct after calling this function.
 *
 * @see su_xml_context_t
 * @see su_xml_free
 */
void su_xml_init(su_xml_context_t *ctx);

/*!
 * @brief Clean up the `su_xml_context_t` structure.
 *
 * Frees any dynamically allocated data in `su_xml_context_t`.
 *
 * @see su_xml_context_t
 * @see su_xml_init
 */
void su_xml_free(su_xml_context_t *ctx);

/*!
 * @brief Output a xml escaped string
 *
 * Outputs the given string escaped for use with XML. It only
 * does minimal-ish escaping, i. e. it escapes all characters
 * that have some syntactical meaning in XML. That includes:
 * Angled brackets (lower than and greater than), ampersand,
 * and single as well as double quotes. All other characters
 * are passed through as is and the caller is expected to
 * make sure they are correctly encoded, i. e. valid UTF-8
 * characters.
 *
 * The escaping is not as minimal as possible. In some cases
 * you can omit escaping all characters except for `<` and `&`,
 * but this would be context-sensitive and therefore
 * unnecessarily tedious to implement while there are no
 * significant downside to this approach.
 *
 * @see su_xml_raw
 */
void su_xml_escaped(su_xml_context_t *ctx, const char *str);

/*!
 * @brief Output a raw string.
 *
 * Output string to `ctx->out`, equivalent to `fputs(str, ctx.out)`.
 * If your string is not valid XML, use xml_escaped() to output it
 * correcty escaped.
 *
 * @see su_xml_context_t
 * @see su_xml_escaped
 */
void su_xml_raw(su_xml_context_t *ctx, const char *str);

/*!
 * @brief Output an empty xml tag.
 *
 * Output an empty xml tag (i. e. a single tag that doesn't need to be closed).
 *
 * The call also outputs given attributes: For `attr_count` n, su_xml_empty_tag() expects
 * 2n additional arguments — for each attribute a name and a value. If value is `NULL`,
 * an attribute without a value will be output, i. e. just the name without the `="…"`.
 *
 * For example, `su_xml_empty_tag(&ctx, "my-tag", 2, "foo", "bar", "baz", NULL);` gives
 * `<my-tag foo="bar" baz/>` with default settings.
 *
 * The attributes' values are XML-escaped automatically. For details on how escaping
 * works in xml.h, see su_xml_escaped().
 *
 * If `closing_slash` is 0 in `ctx`, the slash before the closing ">" will be omitted.
 * This is useful for HTML5 where it is optional.
 *
 * @see su_xml_context_t
 */
void su_xml_empty_tag(su_xml_context_t *ctx, const char *tag, size_t attr_count, ...);

/*!
 * @brief Output an opening tag with attributes.
 *
 * Output an opening tag with attributes and add it to `ctx->stack` for future reference.
 *
 * Attributes work exactly like in `xml_empty_tag()`.
 * @see su_xml_empty_tag
 */
void su_xml_open_tag_attrs(su_xml_context_t *ctx, const char *tag, size_t attr_count, ...);

/*!
 * @brief Output an opening tag without any attributes.
 *
 * Shorthand for `xml_open_tag_attrs(ctx, tag, 0)`.
 *
 * @see su_xml_open_tag_attrs
 */
void su_xml_open_tag(su_xml_context_t *ctx, const char *tag);

/*!
 * @brief Close a previously opened tag.
 *
 * su_xml_close_tag() first checks the head of the current `su_xml_stack_t`
 * if the provided `tag` is in fact the current innermost opened tag.
 *
 * If this is true, it outputs the closing tag, removes the reference
 * to the tag on top of the `su_xml_stack_t` and frees this part of the
 * structure.
 *
 * If not, it will refuse to output the closing tag it was instructed to.
 * This sanity checking of tag closing ensures that a xml document
 * constructed by this module has no nesting errors, i. e. every tag
 * is closed at the proper nesting level. Because it is only simple
 * runtime checking in specific calls it can't prevent / detect the
 * following errors:
 *
 * * It can't prevent unclosed tags remaining at the end. You can
 *   however prevent this by calling su_xml_close_all() at the
 *   end of your XML outputting code.
 * * It can worsen a situation with remaining unclosed tags: If
 *   an inner tag is left unclosed, it will refuse to close all
 *   outer tags, leaving a trail of unclosed tags behind.
 * * It will always attribute the error to closing: Some errors
 *   will be caused by missing an su_xml_open_tag() somewhere, but
 *   su_xml_close_tag() will think the closing is erroneous.
 *   Of course it is also unable to resolve the error.
 * * It can't compare against the intended XML structure: Sometimes
 *   a programming error will result in an "wrong" XML structure
 *   which is still completely valid to su_xml_close_tag(),
 *   i. e. correctly nested.
 *
 * @see su_xml_close_all
 * @see su_xml_close_including
 */
void su_xml_close_tag(su_xml_context_t *ctx, const char *tag);

/*!
 * @brief Close all remaining unclosed tags
 *
 * su_xml_close_all() iterates through the `su_xml_stack_t` and calls
 * su_xml_close_tag() or xml_close_cdata() respectively for every
 * entry in it. A call to it will thus result in an empty `su_xml_stack_t`
 * and all previously opened tags being closed correctly.
 *
 * Internally it's an alias for `su_xml_close_including(ctx, NULL)`
 *
 * Note that `su_xml_close_all()` will limit error checking, since it
 * (by nature) always succeeds and has no insight into what the
 * programmer thinks needs to be closed.
 *
 * @see su_xml_close_tag
 * @see su_xml_close_including
 */
void su_xml_close_all(su_xml_context_t *ctx);

/*!
 * @brief Close all unclosed tags until a given one.
 *
 * su_xml_close_including() works like su_xml_close_all(),
 * but will stop after it hits a tag of the given name.
 * If the given tag is not present in the stack, it behaves
 * like su_xml_close_all(). It is not possible to match
 * a `CDATA` section using xml_close_including().
 *
 * Be aware that it might lead to unexpected results if
 * multiple tags of the same are nested. Consider the
 * following snippet.
 *
 * ```c
 * su_xml_open_tag(&ctx, "a");
 * su_xml_open_tag(&ctx, "b");
 * su_xml_close_tag(&ctx, "a");
 * su_xml_open_tag(&ctx, "c");
 * su_xml_raw(&ctx, "value");
 * su_xml_close_including(&ctx, "a");
 * ```
 *
 * su_xml_close_including() will stop as soon as it hits the first
 * tag `"a"`, although it might be intended to keep going until the
 * outermost one. The result would be:
 *
 * ```xml
 * <a><b><a><c>value</c></a>
 * ```
 *
 * This function will also limit error detection like su_xml_close_all().
 * For an explanation of this, see its documentation.
 *
 * @see su_xml_close_all
 * @see su_xml_close_tag
 */
void su_xml_close_including(su_xml_context_t *ctx, const char *tag);

/*!
 * @brief Start CDATA section
 *
 * Behaves like xml_open_tag(), but for opening `CDATA` sections.
 *
 * Note that this function won't prevent `CDATA` sections or XML
 * elements inside a `CDATA` section, since this is sometimes
 * useful.
 *
 * @see su_xml_close_cdata
 */
void su_xml_open_cdata(su_xml_context_t *ctx);

/*!
 * @brief Close CDATA section
 *
 * Behaves like xml_close_tag(), but for `CDATA` sections.
 *
 * Checks the top of the stack if it is a `CDATA` section.
 * In that case closes it and updates the stack, otherwise
 * does nothing and if applicable outputs a warning.
 *
 * @see su_xml_open_cdata
 */
void su_xml_close_cdata(su_xml_context_t *ctx);
