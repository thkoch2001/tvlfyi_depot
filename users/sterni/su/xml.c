#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <su/xml/render.h>

enum su_xml_tag_type {
    XML_NORMAL_TAG,
    XML_CDATA
};

typedef struct su_xml_stack {
    enum su_xml_tag_type type; //! type of the tag
    char *tag;                 //!< tag name if `XML_NORMAL_TAG`, otherwise `NULL`
    su_xml_stack_t *next;      //!< tag to be closed after the current one
} su_xml_stack_t;

void su_xml_stack_free(su_xml_stack_t *stack) {
    if(stack == NULL) {
        return;
    }

    if(stack->tag != NULL) {
        free(stack->tag);
    }

    if(stack->next != NULL) {
        su_xml_stack_free(stack->next);
    }

    free(stack);
}

void su_xml_init(su_xml_context_t *ctx) {
    ctx->stack = NULL;
    ctx->out = stdout;
    ctx->closing_slash = 1;
    ctx->debug = 0;
}

void su_xml_free(su_xml_context_t *ctx) {
    if(ctx->stack != NULL) {
        su_xml_stack_free(ctx->stack);
    }
}

void output_xml_escaped_char(FILE *out, char c) {
    switch(c) {
        case '&':
            fputs("&amp;", out);
            break;
        case '<':
            fputs("&lt;", out);
            break;
        case '>':
            fputs("&gt;", out);
            break;
        case '\'':
            fputs("&apos;", out);
            break;
        case '\"':
            fputs("&quot;", out);
            break;
        default:
            fputc(c, out);
            break;
    }
}

void su_xml_escaped(su_xml_context_t *ctx, const char *str) {
    for(size_t i = 0; str[i] != '\0'; i++) {
        output_xml_escaped_char(ctx->out, str[i]);
    }
}

void xml_raw(su_xml_context_t *ctx, const char *str) {
    fputs(str, ctx->out);
}

void output_attrs(FILE *out, va_list attrs, size_t arg_count) {
    if(arg_count > 0) {
        for(size_t i = 1; i<=arg_count; i++) {
            if(i % 2) {
                char *name = va_arg(attrs, char *);
                if(name == NULL) {
                    break;
                }

                fputc(' ', out);
                fputs(name, out);
            } else {
                char *maybe_val = va_arg(attrs, char *);
                if(maybe_val != NULL) {
                    fputs("=\"", out);
                    for(size_t i = 0; maybe_val[i] != '\0'; i++) {
                        output_xml_escaped_char(out, maybe_val[i]);
                    }
                    fputc('\"', out);
                }
            }
        }
    }
}

void su_xml_empty_tag(su_xml_context_t *ctx, const char *tag, size_t attr_count, ...) {
    if(tag == NULL || ctx == NULL) {
        return;
    }

    fputc('<', ctx->out);
    fputs(tag, ctx->out);

    if(attr_count > 0) {
        size_t arg_count = attr_count * 2;

        va_list attrs;
        va_start(attrs, attr_count);

        output_attrs(ctx->out, attrs, arg_count);

        va_end(attrs);
    }

    if(ctx->closing_slash) {
        fputc('/', ctx->out);
    }

    fputc('>', ctx->out);
}

void su_xml_open_tag_attrs(su_xml_context_t *ctx, const char *tag, size_t attr_count, ...) {
    if(tag == NULL || ctx == NULL) {
        return;
    }

    su_xml_stack_t *old_stack = ctx->stack;

    fputc('<', ctx->out);
    fputs(tag, ctx->out);


    if(attr_count > 0) {
        size_t arg_count = attr_count * 2;

        va_list attrs;
        va_start(attrs, attr_count);

        output_attrs(ctx->out, attrs, arg_count);

        va_end(attrs);
    }

    fputc('>', ctx->out);

    ctx->stack = malloc(sizeof(su_xml_stack_t));

    if(ctx->stack == NULL) {
        ctx->stack = old_stack;
        return;
    }

    ctx->stack->next = old_stack;

    size_t tag_size = strlen(tag) + 1;
    ctx->stack->type = XML_NORMAL_TAG;
    ctx->stack->tag = malloc(sizeof(char) * tag_size);
    memcpy(ctx->stack->tag, tag, tag_size);
}

void su_xml_open_tag(su_xml_context_t *ctx, const char *tag) {
    su_xml_open_tag_attrs(ctx, tag, 0);
}

void su_xml_close_tag(su_xml_context_t *ctx, const char *tag) {
    if(tag == NULL || ctx == NULL) {
        return;
    }

    if(ctx->stack == NULL) {
        if(ctx->debug)
            fprintf(stderr, "error: no open tags remain, refusing to close tag %s\n", tag);
        return;
    }

    if(ctx->stack->type != XML_NORMAL_TAG) {
        if(ctx->debug)
            fprintf(stderr, "error: expected to close CDATA section, got tag %s\n", tag);
        return;
    }

    if(strcmp(tag, ctx->stack->tag) != 0) {
        if(ctx->debug)
            fprintf(stderr, "error: expected to close tag %s, got tag %s\n", ctx->stack->tag, tag);
        return;
    }

    fputs("</", ctx->out);
    fputs(tag, ctx->out);
    fputc('>', ctx->out);

    su_xml_stack_t *old_head = ctx->stack;

    ctx->stack = old_head->next;

    free(old_head->tag);
    free(old_head);
}

void su_xml_close_all(su_xml_context_t *ctx) {
    su_xml_close_including(ctx, NULL);
}

void su_xml_close_including(su_xml_context_t *ctx, const char *tag) {
    if(ctx == NULL) {
        return;
    }

    if(ctx->stack == NULL) {
        if(ctx->debug && tag != NULL)
            fprintf(stderr, "error: no open tags remain, refusing to close including %s\n", tag);
        return;
    } else {
        int last_tag = tag != NULL && strcmp(tag, ctx->stack->tag) == 0;

        switch(ctx->stack->type) {
            case XML_NORMAL_TAG:
                su_xml_close_tag(ctx, ctx->stack->tag);
                break;
            case XML_CDATA:
                su_xml_close_cdata(ctx);
                break;
            default:
                return;
        }

        if(!last_tag) {
            su_xml_close_including(ctx, tag);
        }
    }
}

void su_xml_open_cdata(su_xml_context_t *ctx) {
    if(ctx == NULL) {
        return;
    }

    su_xml_stack_t *old_stack = ctx->stack;

    ctx->stack = malloc(sizeof(su_xml_stack_t));

    if(ctx->stack == NULL) {
        ctx->stack = old_stack;
        return;
    }

    ctx->stack->next = old_stack;
    ctx->stack->tag = NULL;
    ctx->stack->type = XML_CDATA;

    fputs("<![CDATA[", ctx->out);
}

void su_xml_close_cdata(su_xml_context_t *ctx) {
    if(ctx == NULL) {
        return;
    }

    if(ctx->stack == NULL) {
        if(ctx->debug)
            fputs("error: no open tags remain, refusing to close CDATA section\n", stderr);
        return;
    }

    if(ctx->stack->type != XML_CDATA) {
        if(ctx->debug)
            fprintf(stderr, "error: expected to close CDATA section, got tag %s\n", ctx->stack->tag);
        return;
    }

    su_xml_stack_t *old_head = ctx->stack;

    ctx->stack = old_head->next;

    if(old_head->tag != NULL) {
        // shouldn't happen though
        free(old_head->tag);
    }

    free(old_head);

    fputs("]]>", ctx->out);
}
