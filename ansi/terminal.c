#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <unistd.h>

typedef struct {
  int width;
  int height;
} rect;

static volatile sig_atomic_t resize_pending = 0;

struct winsize _ioctl_winsz() {
  struct winsize ws;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);
  return ws;
}

CAMLprim value edml_get_terminal_size(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(result, pair);

  struct winsize ws = _ioctl_winsz();
  result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(ws.ws_col));
  Store_field(result, 1, Val_int(ws.ws_row));

  CAMLreturn(result);
}

void handle_winch(int sig) {
  resize_pending = 1;
}

CAMLprim value edml_set_resize_callback(value unit) {
  CAMLparam1(unit);
  struct sigaction sa;

  sa.sa_handler = handle_winch;
  sa.sa_flags = 0;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGWINCH, &sa, NULL);

  CAMLreturn(Val_unit);
}

CAMLprim value edml_check_resize(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);

  if (resize_pending) {
    struct winsize ws = _ioctl_winsz();
    resize_pending = 0;

    result = caml_alloc_tuple(2);
    Store_field(result, 0, Val_int(ws.ws_col));
    Store_field(result, 1, Val_int(ws.ws_row));
  } else {
    result = Val_unit;
  }

  CAMLreturn(result);
}
