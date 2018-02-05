// single-precision floating point number using Int32

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <math.h>

CAMLprim value addf32(value a, value b) {
  float ans = *(float *)&Int32_val(a) + *(float *)&Int32_val(b);
  return caml_copy_int32(*(int32_t *)&(ans));
}

CAMLprim value subf32(value a, value b) {
  float ans = *(float *)&Int32_val(a) - *(float *)&Int32_val(b);
  return caml_copy_int32(*(int32_t *)&(ans));
}


CAMLprim value mulf32(value a, value b) {
  float ans = *(float *)&Int32_val(a) * *(float *)&Int32_val(b);
  return caml_copy_int32(*(int32_t *)&(ans));
}

CAMLprim value divf32(value a, value b) {
  float ans = *(float *)&Int32_val(a) / *(float *)&Int32_val(b);
  return caml_copy_int32(*(int32_t *)&(ans));
}

CAMLprim value float32_of_int(value a) {
  float af = (float)Int32_val(a);
  return caml_copy_int32(*(int32_t *)&af);
}

CAMLprim value abs_float32(value a) {
  float af = *(float *)&Int32_val(a);
  float ans = af > 0.0f ? af : -af;
  return caml_copy_int32(*(int32_t *)&ans);
}

CAMLprim value sqrt32(value a) {
  float af = *(float *)&Int32_val(a);
  float ans = sqrtf(af);
  return caml_copy_int32(*(int32_t *)&ans);
}

CAMLprim value round_even32(value a) {
  float af = *(float *)&Int32_val(a);
  float d = af - floor(af);
  af = af - d;
  if (d >= 0.5)
    af += 1.0f;
  return caml_copy_int32(*(int32_t *)&af);
}