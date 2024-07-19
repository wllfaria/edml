#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <tree_sitter/api.h>

CAMLprim value caml_ts_parser_new(value unit) {
  CAMLparam1(unit);
  TSParser *parser = ts_parser_new();
  CAMLreturn((value)parser);
}

CAMLprim value caml_ts_parser_delete(value parser) {
  CAMLparam1(parser);
  ts_parser_delete((TSParser *)parser);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_parser_language(value parser) {
  CAMLparam1(parser);
  const TSLanguage *lng = ts_parser_language((TSParser *)parser);
  CAMLreturn((value)lng);
}

CAMLprim value caml_ts_parser_set_language(value parser, value language) {
  CAMLparam2(parser, language);
  bool ok = ts_parser_set_language((TSParser *)parser, (TSLanguage *)language);
  CAMLreturn((value)ok);
}

CAMLprim bool caml_ts_parser_set_included_ranges(value parser, value ranges, value count) {
  CAMLparam3(parser, ranges, count);
  bool ok = ts_parser_set_included_ranges((TSParser *)parser, (TSRange *)ranges, (uint32_t)count);
  CAMLreturn((value)ok);
}

CAMLprim value caml_ts_parser_included_ranges(value parser, value count) {
  CAMLparam2(parser, count);
  const TSRange *range = ts_parser_included_ranges((TSParser *)parser, (uint32_t *)count);
  CAMLreturn((value)range);
}

CAMLprim value caml_ts_parser_parse(value parser, value old_tree, value input) {
  CAMLparam3(parser, old_tree, input);
  TSTree *tree = ts_parser_parse((TSParser *)parser, (TSTree *)old_tree, *(TSInput *)input);
  CAMLreturn((value)tree);
}

CAMLprim value caml_ts_parser_parse_string(value parser, value old_tree, value string, value length) {
  CAMLparam4(parser, old_tree, string, length);
  TSTree *tree = ts_parser_parse_string((TSParser *)parser, (TSTree *)old_tree, (char *)string, (uint32_t)length);
  CAMLreturn((value)tree);
}
