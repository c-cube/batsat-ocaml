use chlorine::*;

#[allow(unused)]
use chlorine::c_void;

#[test]
fn all_the_types_are_present() {
  let _: c_char = 0;
  let _: c_schar = 0;
  let _: c_uchar = 0;
  let _: c_short = 0;
  let _: c_ushort = 0;
  let _: c_int = 0;
  let _: c_uint = 0;
  let _: c_long = 0;
  let _: c_ulong = 0;
  let _: c_longlong = 0;
  let _: c_ulonglong = 0;
  let _: c_float = 0.0;
  let _: c_double = 0.0;

  pick! {
    if #[cfg(feature = "int_extras")] {
      let _: intmax_t = 0;
      let _: intptr_t = 0;
      let _: ptrdiff_t = 0;
      let _: size_t = 0;
      let _: ssize_t = 0;
      let _: uintmax_t = 0;
      let _: uintptr_t = 0;
    }
  }
}

#[test]
fn all_the_types_match_std_os_raw() {
  let a: c_char = 0;
  let b: std::os::raw::c_char = 0;
  assert_eq!(a, b);
  let a: c_schar = 0;
  let b: std::os::raw::c_schar = 0;
  assert_eq!(a, b);
  let a: c_uchar = 0;
  let b: std::os::raw::c_uchar = 0;
  assert_eq!(a, b);
  let a: c_short = 0;
  let b: std::os::raw::c_short = 0;
  assert_eq!(a, b);
  let a: c_ushort = 0;
  let b: std::os::raw::c_ushort = 0;
  assert_eq!(a, b);
  let a: c_int = 0;
  let b: std::os::raw::c_int = 0;
  assert_eq!(a, b);
  let a: c_uint = 0;
  let b: std::os::raw::c_uint = 0;
  assert_eq!(a, b);
  let a: c_long = 0;
  let b: std::os::raw::c_long = 0;
  assert_eq!(a, b);
  let a: c_ulong = 0;
  let b: std::os::raw::c_ulong = 0;
  assert_eq!(a, b);
  let a: c_longlong = 0;
  let b: std::os::raw::c_longlong = 0;
  assert_eq!(a, b);
  let a: c_ulonglong = 0;
  let b: std::os::raw::c_ulonglong = 0;
  assert_eq!(a, b);
  let a: c_float = 0.0;
  let b: std::os::raw::c_float = 0.0;
  assert_eq!(a, b);
  let a: c_double = 0.0;
  let b: std::os::raw::c_double = 0.0;
  assert_eq!(a, b);
  // the other types are not in std::os::raw
}
