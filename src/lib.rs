extern crate batsat;
#[macro_use]
extern crate ocaml;

#[link(name="batsat")]

use {
    ocaml::{ToValue,Value,value },
    std::{ptr, mem, ops, },
    batsat::{Var,Lit,lbool,SolverInterface,BasicSolver as SatSolver,TheoryArg},
};

struct Theory<'a> {
    value: &'a Value, // OCaml record of functions
    lits: &'a mut Vec<Lit>,
}

struct Solver {
    s: SatSolver,
    lits: Vec<Lit>,
    cur_clause: Vec<Lit>,
    assumptions: Vec<Lit>,
}

/// Convert a literal into a signed integer for the OCaml frontend
// NOTE: we add 1 to prevent the first variable from having index 0
#[inline]
fn int_of_lit(lit: Lit) -> isize {
    (lit.var().idx() as isize + 1) * if lit.sign() { 1 } else { -1 }
}

#[inline]
fn lit_of_int(lit: i32) -> Lit {
    assert!(lit != 0);
    let v = Var::unsafe_from_idx((lit.abs() - 1) as u32);
    Lit::new(v, lit>0)
}

impl Solver {
    fn new() -> Self {
        let s = SatSolver::default();
        Solver {
            s, lits: vec!(), cur_clause: vec![], assumptions: vec![],
        }
    }
}

impl Solver {
    #[inline]
    fn get_lit(&mut self, lit: i32) -> Lit {
        assert!(lit != 0);
        let v = self.s.var_of_int((lit.abs()-1) as u32);
        Lit::new(v, lit>0)
    }
}

impl ops::Deref for Solver {
    type Target = SatSolver;
    fn deref(&self) -> &Self::Target { &self.s }
}

impl ops::DerefMut for Solver {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.s }
}

// NOTE on storage:
// we use an OCaml custom block to store the pointer to the Solver (not the
// solver itself).

// macro to locally borrow solver. `with_solver!(s, v, block)`
// runs `block` in a context where `s` binds to a `&mut solver` from `v`
macro_rules! with_solver {
    ($s: ident, $v:expr, $code:block) => {
        {
            assert!($v.custom_ptr_val::<* const Solver>() != ptr::null());
            let $s : &mut Solver = &mut (**$v.custom_ptr_val_mut::<*mut Solver>());
            $code;
        };
    }
}

// NOTE: must mirror the order of fields in `batsat.ml/Theory`
#[repr(usize)]
enum RecordFields {
    State = 0,
    NLevels,
    PushLevel,
    PopLevels,
    HasPartialCheck,
    PartialCheck,
    FinalCheck,
    ExplainProp,
}

/// Unwap the result of an OCaml function call, by re-raising the exception
#[inline]
fn unwrap_or_raise(x: Result<Value, ocaml::Error>) -> Value {
    match x {
        Ok(x) => x,
        Err(ocaml::Error::Exception(e)) => {
            //println!("batsat: re-raise exn {:?}", e);
            ocaml::runtime::raise(&e);
            value::UNIT
        },
        Err(e) => {
            panic!("batsat: ocaml call gives non-exn error {:?}", e)
        }
    }
}

/// Finalizer that does nothing
extern "C" fn batsat_no_op_finalizer(_v: ocaml::core::Value) {}

impl<'a> batsat::Theory for Theory<'a> {
    fn final_check(&mut self, acts: &mut TheoryArg) {
        // alloc before dereferencing fields
        let th_act = Value::alloc_custom(acts, batsat_no_op_finalizer);
        let st = self.value.field(RecordFields::State as usize);
        let f = self.value.field(RecordFields::FinalCheck as usize);
        unwrap_or_raise(f.call2_exn(st, th_act));
    }

    fn partial_check(&mut self, acts: &mut TheoryArg) {
        if self.value.field(RecordFields::HasPartialCheck as usize).isize_val() != 0 {
            // alloc before dereferencing fields
            let th_act = Value::alloc_custom(acts, batsat_no_op_finalizer);
            let st = self.value.field(RecordFields::State as usize);
            let f = self.value.field(RecordFields::PartialCheck as usize);
            unwrap_or_raise(f.call2_exn(st, th_act));
        }
    }

    fn create_level(&mut self) {
        let st = self.value.field(RecordFields::State as usize);
        let f = self.value.field(RecordFields::PushLevel as usize);
        unwrap_or_raise(f.call_exn(st));
    }

    fn pop_levels(&mut self, n: usize) {
        let st = self.value.field(RecordFields::State as usize);
        let f = self.value.field(RecordFields::PopLevels as usize);
        let n = Value::isize(n as isize);
        unwrap_or_raise(f.call2_exn(st, n));
    }

    fn n_levels(&self) -> usize {
        let st = self.value.field(RecordFields::State as usize);
        let f = self.value.field(RecordFields::NLevels as usize);
        let n = unwrap_or_raise(f.call_exn(st));
        n.isize_val() as usize
    }

    fn explain_propagation(&mut self, p: Lit) -> &[Lit] {
        let st = self.value.field(RecordFields::State as usize);
        let f = self.value.field(RecordFields::ExplainProp as usize);

        // no alloc from there on
        let lits: ocaml::Array =
            unwrap_or_raise(f.call2_exn(st, Value::isize(int_of_lit(p)))).into();
        let n = lits.len();
        self.lits.clear();
        self.lits.reserve(n);
        for i in 0 .. n - 1 {
            let lit = lit_of_int(lits.get(i).unwrap().isize_val() as i32);
            self.lits.push(lit);
        };
        self.lits.as_slice()
    }
}

fn delete_value(v: Value) {
    if unsafe{ *v.custom_ptr_val::<*const Solver>() } != ptr::null() {
        //println!("delete value");
        let s = unsafe { Box::from_raw(*v.custom_ptr_val_mut::<*mut Solver>()) };
        mem::drop(s); // delete!
    }
    // be sure not to delete twice
    unsafe { * v.custom_ptr_val_mut::<*const Solver>() = ptr::null() };
}

// finalizer for values
extern "C" fn batsat_finalizer(v: ocaml::core::Value) {
    delete_value(Value::new(v));
}

// ### SOLVER

caml!(ml_batsat_new, |_params|, <res>, {
    let solver = Box::new(Solver::new());
    let ptr = Box::into_raw(solver) as *mut Solver;
    res = Value::alloc_custom(ptr, batsat_finalizer);
} -> res);

caml!(ml_batsat_delete, |param|, <res>, {
    delete_value(param);
    res = value::UNIT;
} -> res);

caml!(ml_batsat_simplify, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.simplify().into();
        res = Value::bool(r);
    })
} -> res);

caml!(ml_batsat_get_lit, |ptr, i|, <res>, {
    with_solver!(solver, ptr, {
        let lit = solver.get_lit(i.isize_val() as i32);
        res = Value::isize(int_of_lit(lit));
    })
} -> res);

caml!(ml_batsat_fresh_lit, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let lit = Lit::new(solver.s.new_var_default(), true);
        res = Value::isize(int_of_lit(lit));
    })
} -> res);

/// Add literal, or add clause if the lit is 0
caml!(ml_batsat_addlit, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit.isize_val() as i32;

        let mut r = true;
        if lit == 0 {
            // push current clause into vector `clauses`, reset it
            //println!("add-lit {:?}", 0);
            let Solver {s:solver, cur_clause, ..} = solver;
            r = solver.add_clause_reuse(cur_clause);
            cur_clause.clear();
        } else {
            // push literal into clause
            let lit = lit_of_int(lit);
            //println!("add-lit {:?}", lit);
            solver.cur_clause.push(lit);
        }
        res = Value::bool(r);
    })
} -> res);

/// Add assumption into the solver
caml!(ml_batsat_assume, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        solver.assumptions.push(lit);
        res = value::UNIT;
    })
} -> res);

caml!(ml_batsat_solve, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = {
            let Solver {s, assumptions, ..} = solver;
            let lb = s.solve_limited(&assumptions);
            assumptions.clear(); // reset assumptions
            assert_ne!(lb, lbool::UNDEF); // can't express that in a bool
            lb != lbool::FALSE
        };
        //println!("res: {:?}, model: {:?}", r, solver.get_model());
        res = Value::bool(r);
    })
} -> res);


caml!(ml_batsat_solve_th, |ptr, th|, <res>, {
    with_solver!(solver, ptr, {
        let r = {
            let Solver {s, assumptions, lits, ..} = solver;
            let mut th = Theory{ value: &th, lits };
            let lb = s.solve_limited_th(&mut th, &assumptions);
            assumptions.clear(); // reset assumptions
            assert_ne!(lb, lbool::UNDEF); // can't express that in a bool
            lb != lbool::FALSE
        };
        //println!("res: {:?}, model: {:?}", r, solver.get_model());
        res = Value::bool(r);
    })
} -> res);

caml!(ml_batsat_value, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        let r = solver.s.value_lit(lit);
        //println!("val for {:?}: {:?}", lit, r);
        res = Value::isize(r.to_u8() as isize);

    });
} -> res);

caml!(ml_batsat_value_lvl_0, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        let lit = lit_of_int(lit.isize_val() as i32);
        let r = solver.s.value_lvl_0(lit);
        //println!("val for {:?}: {:?}", lit, r);
        res = Value::isize(r.to_u8() as isize);

    });
} -> res);


caml!(ml_batsat_check_assumption, |ptr, lit|, <res>, {
    with_solver!(solver, ptr, {
        // check unsat-core
        let lit = lit_of_int(lit.isize_val() as i32);
        let r = solver.s.unsat_core_contains_var(lit.var());

        res = Value::bool(r);
    })
} -> res);

caml!(ml_batsat_unsat_core, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let core =
            solver.s.unsat_core()
            .iter()
            .map(|&lit| int_of_lit(lit))
            .collect::<Vec<_>>();
        res = core.to_value();
    })
} -> res);

caml!(ml_batsat_nvars, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_vars() as isize;
        res = Value::isize(r);
    });
} -> res);

caml!(ml_batsat_nclauses, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_clauses();
        res = Value::isize(r as isize);
    })
} -> res);

caml!(ml_batsat_nconflicts, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_conflicts();
        res = Value::isize(r as isize);
    })
} -> res);

caml!(ml_batsat_nprops, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_propagations();
        res = Value::isize(r as isize);
    })
} -> res);

caml!(ml_batsat_ndecisions, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_decisions();
        res = Value::isize(r as isize);
    })
} -> res);

/*
caml!(ml_batsat_nrestarts, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.num_restarts();
        res = Value::isize(r as isize);
    })
} -> res);
*/

caml!(ml_batsat_n_proved, |ptr|, <res>, {
    with_solver!(solver, ptr, {
        let r = solver.s.proved_at_lvl_0().len();
        res = Value::isize(r as isize);
    })
} -> res);

caml!(ml_batsat_get_proved, |ptr, idx|, <res>, {
    let i = idx.isize_val() as usize;
    with_solver!(solver, ptr, {
        let lit = solver.s.proved_at_lvl_0()[i];
        let lit = int_of_lit(lit);
        res = Value::isize(lit);
    })
} -> res);


// ### THEORY ARGUMENT

// macro to locally borrow theory_arg. `with_th_arg!(a, v, block)`
// runs `block` in a context where `a` binds to a `&mut TheoryArg` from `v`
macro_rules! with_th_arg {
    ($a: ident, $v:expr, $code:block) => {
        {
            assert!($v.custom_ptr_val::<* const TheoryArg>() != ptr::null());
            let $a : &mut TheoryArg = &mut (**$v.custom_ptr_val_mut::<*mut TheoryArg>());
            $code;
        };
    }
}

caml!(ml_batsat_arg_value, |ptr_a, lit|, <res>, {
    with_th_arg!(a, ptr_a, {
        let lit = lit_of_int(lit.isize_val() as i32);
        let mut v = a.value(lit.var());
        if ! lit.sign() { v = -v }
        res = Value::isize(v.to_u8() as isize);
    })
} -> res);

caml!(ml_batsat_arg_model_len, |ptr_a|, <res>, {
    with_th_arg!(a, ptr_a, {
        res = Value::isize(a.model().len() as isize);
    })
} -> res);

caml!(ml_batsat_arg_model_get, |ptr_a, i|, <res>, {
    with_th_arg!(a, ptr_a, {
        let lit = a.model()[i.isize_val() as usize];
        res = Value::isize(int_of_lit(lit) as isize);
    })
} -> res);

caml!(ml_batsat_arg_mk_lit, |ptr_a|, <res>, {
    with_th_arg!(a, ptr_a, {
        let lit = a.mk_new_lit();
        res = Value::isize(int_of_lit(lit) as isize);
    })
} -> res);

caml!(ml_batsat_arg_raise_conflict, |ptr_a, c, costly|, <res>, {
    with_th_arg!(a, ptr_a, {
        if a.is_ok() {
            let c: ocaml::Array = c.into();
            let n = c.len();
            let mut clause = Vec::with_capacity(n);
            for i in 0 .. n {
                let val = unwrap_or_raise(c.get(i));
                clause.push(lit_of_int(val.isize_val() as i32));
            }
            a.raise_conflict(&clause, costly.isize_val() != 0);
        }
        res = value::UNIT;
    })
} -> res);

caml!(ml_batsat_arg_push_lemma, |ptr_a, c|, <res>, {
    with_th_arg!(a, ptr_a, {
        if a.is_ok() {
            let c: ocaml::Array = c.into();
            let n = c.len();
            let mut clause = Vec::with_capacity(n);
            for i in 0 .. n {
                let val = unwrap_or_raise(c.get(i));
                clause.push(lit_of_int(val.isize_val() as i32));
            }
            a.add_theory_lemma(&clause);
        }
        res = value::UNIT;
    })
} -> res);

caml!(ml_batsat_arg_propagate, |ptr_a, lit|, <res>, {
    with_th_arg!(a, ptr_a, {
        if a.is_ok() {
            let lit = lit_of_int(lit.isize_val() as i32);
            a.propagate(lit);
        }
        res = value::UNIT;
    })
} -> res);
