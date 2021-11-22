#![no_std]
#![allow(non_camel_case_types)]

//! This just provides the numeric C types, for basic FFI purposes.
//!
//! Also, that [`pick!`] macro is nifty.

/// Does all our conditional compilation selection.
#[macro_export]
macro_rules! pick {
  // with a trailing else
  ($(if #[cfg($($test:meta),*)] {
      $($if_tokens:tt)*
    })else+ else {
      $($else_tokens:tt)*
    }) => {
    $crate::pick!{
      @__forests [ ] ;
      $( [ {$($test),*} {$($if_tokens)*} ], )*
      [ { } {$($else_tokens)*} ],
    }
  };

  // without a trailing else
  (if #[cfg($($if_meta:meta),*)] {
      $($if_tokens:tt)*
    } $(else if #[cfg($($else_meta:meta),*)] {
      $($else_tokens:tt)*
    })*) => {
    $crate::pick!{
      @__forests [ ] ;
      [ {$($if_meta),*} {$($if_tokens)*} ],
      $( [ {$($else_meta),*} {$($else_tokens)*} ], )*
    }
  };

  // private
  (@__forests [$($not:meta,)*];) => {
    /* halt expansion */
  };

  // private
  (@__forests [$($not:meta,)*]; [{$($m:meta),*} {$($tokens:tt)*}], $($rest:tt)*) => {
    // This "one weird trick" works because you can't apply a `cfg` to an
    // expression, only an item or a block, but a macro usage is an item, so
    // we're configuring the macro usage, which (if configured in) will then
    // contain a token tree that turns into either an item or an expression.
    #[cfg(all( $($m,)* not(any($($not),*)) ))]
    $crate::pick!{ @__identity $($tokens)* }

    $crate::pick!{ @__forests [ $($not,)* $($m,)* ] ; $($rest)* }
  };

  // private
  (@__identity $($tokens:tt)*) => {
    $($tokens)*
  };
}

// most of these never change

pub use core::ffi::c_void;
pub type c_schar = i8;
pub type c_uchar = u8;
// char depends on stuff
pub type c_short = i16;
pub type c_ushort = u16;
// int/uint depends on stuff
// long/ulong depends on stuff
pub type c_longlong = i64;
pub type c_ulonglong = u64;
pub type c_float = f32;
pub type c_double = f64;

// c_char, c_int, and c_uint are set by the arch

// first we define c_char
pick! {
  if #[cfg(any(
    target_arch = "arm",
    target_arch = "asmjs",
    target_arch = "powerpc",
    target_arch = "powerpc64",
    target_arch = "s390x",
    target_arch = "riscv32",
    target_arch = "riscv64",
    target_arch = "aarch64",
    target_arch = "msp430",
  ))] {
    // c_char is unsigned (unless using apple)
    pick! {
      if #[cfg(any(
        target_os = "macos",
        target_os = "ios",
      ))] {
        pub type c_char = c_schar;
      } else {
        pub type c_char = c_uchar;
      }
    }
  } else if #[cfg(any(
    target_arch = "mips",
    target_arch = "mips64",
    target_arch = "sparc64",
    target_arch = "x86",
    target_arch = "x86_64",
    target_arch = "nvptx",
    target_arch = "nvptx64",
    target_arch = "xtensa",
    target_arch = "wasm32",
    target_arch = "wasm64",
  ))] {
    // c_char is signed
    pub type c_char = c_schar;
  } else {
    compile_error!("The alias for c_char, c_int, and c_uint is unknown!");
  }
}

// then we define c_int and c_uint
pick! {
  if #[cfg(any(
    target_arch = "arm",
    target_arch = "asmjs",
    target_arch = "wasm32",
    target_arch = "wasm64",
    target_arch = "powerpc",
    target_arch = "powerpc64",
    target_arch = "s390x",
    target_arch = "riscv32",
    target_arch = "riscv64",
    target_arch = "aarch64",
    target_arch = "mips",
    target_arch = "mips64",
    target_arch = "sparc64",
    target_arch = "x86",
    target_arch = "x86_64",
    target_arch = "nvptx",
    target_arch = "nvptx64",
    target_arch = "xtensa",
  ))] {
    pub type c_int = i32;
    pub type c_uint = u32;
  } else if #[cfg(any(
    target_arch = "msp430",
  ))] {
    pub type c_int = i16;
    pub type c_uint = u16;
  } else {
    compile_error!("The alias for c_int and c_uint is unknown!");
  }
}

// c_long and c_ulong are set by the OS
pick! {
  // in some special cases we ignore pointer size ...
  if #[cfg(windows)] {
    pub type c_long = i32;
    pub type c_ulong = u32;
  } else if #[cfg(any(
    target_os = "redox",
    target_os = "solaris",
  ))] {
    pub type c_long = i64;
    pub type c_ulong = u64;
  }
  // but by default we match the size of a long to the size of a pointer
  else if #[cfg(target_pointer_width = "64")] {
    pub type c_long = i64;
    pub type c_ulong = u64;
  } else if #[cfg(any(
    target_pointer_width = "16",
    target_pointer_width = "32",
  ))] {
    pub type c_long = i32;
    pub type c_ulong = u32;
  } else {
    compile_error!("The alias for c_long and c_ulong is unknown!");
  }
}

// this requires a crate feature is all
pick! {
  if #[cfg(feature = "int_extras")] {
    pub type intmax_t = i64;
    pub type intptr_t = isize;
    pub type ptrdiff_t = isize;
    pub type size_t = usize;
    pub type ssize_t = isize;
    pub type uintmax_t = u64;
    pub type uintptr_t = usize;
  }
}
