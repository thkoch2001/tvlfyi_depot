#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <su/xml/emit.h>

#define DEBUG_WARN(ctx, ...) \
    if(ctx->warn != NULL) { \
        fprintf(ctx->warn, __VA_ARGS__); \
    }

void debug_xml_stack(FILE *out, struct xml_stack *stack) {
    if(stack != NULL) {
        fprintf(out, "%s ", stack->tag);
        debug_xml_stack(out, stack->next);
    } else {
        fputc('\n', out);
    }
}

void free_xml_stack(struct xml_stack *stack) {
    if(stack == NULL) {
        return;
    }

    if(stack->tag != NULL) {
        free(stack->tag);
    }

    if(stack->next != NULL) {
        free_xml_stack(stack->next);
    }

    free(stack);
}

void new_xml_context(struct xml_context *ctx) {
    ctx->stack = NULL;
    ctx->warn = NULL;
    ctx->out = stdout;
    ctx->closing_slash = 1;
}

void del_xml_context(struct xml_context *ctx) {
    if(ctx->stack != NULL) {
        if(ctx->warn != NULL) {
            fputs("Unclosed tags remaining: ", ctx->warn);
            debug_xml_stack(ctx->warn, ctx->stack);
        }

        free_xml_stack(ctx->stack);
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

void xml_escaped(struct xml_context *ctx, const char *str) {
    for(size_t i = 0; str[i] != '\0'; i++) {
        output_xml_escaped_char(ctx->out, str[i]);
    }
}

void xml_raw(struct xml_context *ctx, const char *str) {
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

void xml_empty_tag(struct xml_context *ctx, const char *tag, size_t attr_count, ...) {
    if(tag == NULL || ctx == NULL) {
        DEBUG_WARN(ctx, "Got no tag or ctx\n");
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

void xml_open_tag_attrs(struct xml_context *ctx, const char *tag, size_t attr_count, ...) {
    if(tag == NULL || ctx == NULL) {
        DEBUG_WARN(ctx, "Got no tag or ctx\n");
        return;
    }

    struct xml_stack *old_stack = ctx->stack;

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

    ctx->stack = malloc(sizeof(struct xml_context));

    if(ctx->stack == NULL) {
        ctx->stack = old_stack;
        DEBUG_WARN(ctx, "Could not allocate memory for tag stack, now everything will break.\n")
        return;
    }

    ctx->stack->next = old_stack;

    size_t tag_size = strlen(tag) + 1;
    ctx->stack->type = XML_NORMAL_TAG;
    ctx->stack->tag = malloc(sizeof(char) * tag_size);
    memcpy(ctx->stack->tag, tag, tag_size);
}

void xml_open_tag(struct xml_context *ctx, const char *tag) {
    xml_open_tag_attrs(ctx, tag, 0);
}

void xml_close_tag(struct xml_context *ctx, const char *tag) {
    if(tag == NULL || ctx == NULL) {
        DEBUG_WARN(ctx, "Got no tag or ctx\n");
        return;
    }

    if(ctx->stack == NULL) {
        DEBUG_WARN(ctx, "Refusing to close tag %s, no tags left to be closed\n", tag);
        return;
    }

    if(ctx->stack->type != XML_NORMAL_TAG) {
        DEBUG_WARN(ctx, "Refusing to close tag %s, wrong tag type\n", tag);
        return;
    }

    if(strcmp(tag, ctx->stack->tag) != 0) {
        DEBUG_WARN(ctx, "Refusing to close tag %s, unclosed tags remaining\n", tag);
        return;
    }

    fputs("</", ctx->out);
    fputs(tag, ctx->out);
    fputc('>', ctx->out);

    struct xml_stack *old_head = ctx->stack;

    ctx->stack = old_head->next;

    free(old_head->tag);
    free(old_head);
}

void xml_close_all(struct xml_context *ctx) {
    xml_close_including(ctx, NULL);
}

void xml_close_including(struct xml_context *ctx, const char *tag) {
    if(ctx == NULL) {
        DEBUG_WARN(ctx, "Got no ctx\n");
        return;
    }

    if(ctx->stack == NULL) {
        if(tag != NULL) {
            DEBUG_WARN(ctx, "Hit end of tag stack while searching for tag %s to close\n", tag);
        }
        return;
    } else {
        int last_tag = tag != NULL && strcmp(tag, ctx->stack->tag) == 0;

        switch(ctx->stack->type) {
            case XML_NORMAL_TAG:
                xml_close_tag(ctx, ctx->stack->tag);
                break;
            case XML_CDATA:
                xml_close_cdata(ctx);
                break;
            default:
                DEBUG_WARN(ctx, "Unexpected tag type on stack, aborting\n");
                return;
        }

        if(!last_tag) {
            xml_close_including(ctx, tag);
        }
    }
}

void xml_open_cdata(struct xml_context *ctx) {
    if(ctx == NULL) {
        DEBUG_WARN(ctx, "Got no ctx\n");
        return;
    }

    struct xml_stack *old_stack = ctx->stack;

    ctx->stack = malloc(sizeof(struct xml_stack));

    if(ctx->stack == NULL) {
        ctx->stack = old_stack;

        DEBUG_WARN(ctx, "Could not allocate memory for tag stack, now everything will break.\n");
        return;
    }

    ctx->stack->next = old_stack;
    ctx->stack->tag = NULL;
    ctx->stack->type = XML_CDATA;

    fputs("<![CDATA[", ctx->out);
}

void xml_close_cdata(struct xml_context *ctx) {
    if(ctx == NULL) {
        DEBUG_WARN(ctx, "Got no ctx\n");
        return;
    }

    if(ctx->stack == NULL) {
        DEBUG_WARN(ctx, "No CDATA to close\n");
        return;
    }

    if(ctx->stack->type != XML_CDATA) {
        DEBUG_WARN(ctx, "No CDATA on top of stack, refusing to close\n");
        return;
    }

    struct xml_stack *old_head = ctx->stack;

    ctx->stack = old_head->next;

    if(old_head->tag != NULL) {
        // shouldn't happen though
        free(old_head->tag);
    }

    free(old_head);

    fputs("]]>", ctx->out);
}
