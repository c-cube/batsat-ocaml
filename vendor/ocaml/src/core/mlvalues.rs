//! Contains OCaml types and conversion functions from runtime representations.

#[allow(non_camel_case_types)]
pub type Value = usize;
pub type Intnat = isize;
pub type Uintnat = usize;
#[allow(non_camel_case_types)]
pub type Size = Uintnat;
#[allow(non_camel_case_types)]
pub type Tag = u8; //typedef unsigned int tag_t; // Actually, an unsigned char
#[allow(non_camel_case_types)]
pub type Color = Uintnat;
#[allow(non_camel_case_types)]
pub type Mark = Uintnat;

/// An OCaml heap-allocated block header. **NB**: this is currently unimplemented.
///
/// Structure of the header:
///
/// For 16-bit and 32-bit architectures:
///
///```text
///      +--------+-------+-----+
///      | wosize | color | tag |
///      +--------+-------+-----+
/// bits  31    10 9     8 7   0
///```
///
/// For 64-bit architectures:
///
///```text
///      +--------+-------+-----+
///      | wosize | color | tag |
///      +--------+-------+-----+
/// bits  63    10 9     8 7   0
///```
///
pub struct Header {}

/// #ifdef ARCH_BIG_ENDIAN
/// #define Tag_val(val) (((unsigned char *) (val)) [-1])
/// #else
/// #define Tag_val(val) (((unsigned char *) (val)) [-sizeof(value)])
/// #endif
#[cfg(target_endian = "big")]
macro_rules! tag_val {
($x:expr) => (*(($x as *const u8).offset(-1)));
($x:ident) => (*(($x as *const u8).offset(-1)));
}

#[cfg(target_endian = "little")]
macro_rules! tag_val {
($x:expr) => (*(($x as *const u8).offset(-(::std::mem::size_of::<Value>() as isize))));
($x:ident) => (*(($x as *const u8).offset(-(::std::mem::size_of::<Value>() as isize))));
}

macro_rules! hd_val {
($x:expr) => (*(($x as *const usize).offset(-1)));
($x:ident) => (*(($x as *const usize).offset(-1)));
}

macro_rules! wosize_val {
($x:expr) => (hd_val!($x) >> 10);
($x:ident) => (hd_val!($x) >> 10);
}

/// `(((intnat)(x) << 1) + 1)`
macro_rules! val_long {
($x:expr) => ((($x as usize) << 1) + 1);
($x:ident) => ((($x as usize) << 1) + 1);
}

/// `Long_val(x)     ((x) >> 1)`
macro_rules! long_val {
($x:ident) => ($x as usize >> 1);
($x:expr) => ($x as usize >> 1);
}

/// Converts a machine `usize` into an OCaml `int`
///
/// `Val_int(x) Val_long(x)`
macro_rules! val_int {
($x:expr) => ( val_long!($x) );
($x:ident) => ( val_long!($x) );
}

/// Converts an OCaml `int` into a `usize`
///
/// `Int_val(x) ((int) Long_val(x))`
macro_rules! int_val {
($x:ident) => (long_val!($x));
($x:expr) => (long_val!($x));
}

/// Creates an empty list
pub fn empty_list() -> Value {
    val_int!(0)
}

pub fn is_block(v: Value) -> bool {
    (v & 1) == 0
}

pub fn is_long(v: Value) -> bool {
    (v & 1) != 0
}

// #define Max_long (((intnat)1 << (8 * sizeof(value) - 2)) - 1)
// #define Min_long (-((intnat)1 << (8 * sizeof(value) - 2)))

/// Extracts from the `$block` an OCaml value at the `$ith`-field
macro_rules! field {
    ($block:expr, $i:expr) => (
        ($block as *mut $crate::core::mlvalues::Value).offset($i)
    );
}

pub unsafe fn field(value: Value, i: usize) -> *mut Value {
    field!(value, i as isize)
}

pub unsafe fn as_slice<'a>(value: Value) -> &'a [Value] {
    ::std::slice::from_raw_parts(
        (value as *const Value).offset(-1),
        wosize_val!(value) + 1)
}

/// The OCaml `()` (`unit`) value - rien.
pub const UNIT: Value = val_int!(0);

/// The OCaml `true` value
pub const TRUE: Value = val_int!(1);

/// OCaml `false` value
pub const FALSE: Value = val_int!(0);

// Strings

/// Pointer to the first byte
macro_rules! bp_val {
  ($v: expr) => {
      $v as *const u8
  }
}

/// Extracts a machine `ptr` to the bytes making up an OCaml `string`
macro_rules! string_val {
  ($v:expr) => {
      bp_val!($v)
  }
}

extern "C" {
    /// Returns size of the string in `value` in bytes
    pub fn caml_string_length(value: Value) -> Size;
    pub fn caml_array_length(value: Value) -> Size;
    pub fn caml_hash_variant(tag: *const u8) -> Value;
    pub fn caml_get_public_method(obj: Value, tag: Value) -> Value;
}
