#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>

#include <stddef.h>

// decls

struct solver;
extern struct solver* batsat_solver_alloc(void);
extern void batsat_solver_free(struct solver*);

// stubs

// custom block
struct solver_v {
  struct solver* solver;
};

static void solver_v_free(value val) {
  struct solver_v* v = (struct solver_v*) Data_custom_val(val);
  batsat_solver_free(v->solver);
}

static struct custom_operations solver_ops = {
  .identifier = "batsat_solver",
  .finalize = solver_v_free,
  .compare = NULL,
  .compare_ext = NULL,
  .hash = NULL,
  .serialize = NULL,
  .deserialize = NULL,
  .fixed_length = NULL,
};

CAMLprim value ml_batsat_new(void) {
  CAMLparam0();
  CAMLlocal1(res);
  struct solver* s = batsat_solver_alloc();
  res = caml_alloc_custom(&solver_ops, sizeof(struct solver_v), 10, 200);
  CAMLreturn(res);
}
