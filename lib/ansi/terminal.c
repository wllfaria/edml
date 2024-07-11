#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <sys/ioctl.h>
#include <unistd.h>

typedef struct {
  int width;
  int height;
} rect;

CAMLprim value edml_get_terminal_size(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(result, pair);
  struct winsize ws;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);

  result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(ws.ws_col));
  Store_field(result, 1, Val_int(ws.ws_row));

  CAMLreturn(result);
}
