use {
    batsat::{lbool, BasicSolver as InnerSolver, Lit, SolverInterface, Var},
    std::{default::Default, ops},
};

pub struct Solver {
    s: InnerSolver,
    vars: Vec<Var>, // int->var
    cur_clause: Vec<Lit>,
    assumptions: Vec<Lit>,
}

unsafe extern "C" fn batsat_solver_free(p: *mut Solver) {
    //eprintln!("delete value");
    assert!(!p.is_null());
    std::ptr::drop_in_place(p);
}

unsafe extern "C" fn batsat_solver_alloc() -> *mut Solver {
    let s = Solver {
        s: InnerSolver::default(),
        vars: Vec::new(),
        cur_clause: vec![],
        assumptions: vec![],
    };
    let b = Box::new(s);
    Box::into_raw(b)
}
