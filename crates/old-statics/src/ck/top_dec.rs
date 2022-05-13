//! Check top-level declarations.

use crate::ck::util::{env_ins, get_env};
use crate::ck::{dec, sig_match, ty};
use crate::ty_rzn::TyRealization;
use crate::types::{
  Basis, Env, Error, FunEnv, FunSig, Item, Result, Sig, SigEnv, State, StrEnv, Ty, TyEnv, TyInfo,
  TyScheme, ValEnv, ValInfo,
};
use old_ast::{SigExp, Spec, StrDec, StrExp, TopDec};
use old_loc::Located;

pub fn ck(bs: &mut Basis, st: &mut State, top_dec: &Located<TopDec>) -> Result<()> {
  match &top_dec.val {
    // SML Definition (87)
    TopDec::StrDec(str_dec) => {
      let env = ck_str_dec(bs, st, str_dec)?;
      bs.env.extend(env);
    }
    // SML Definition (88)
    TopDec::SigDec(sig_binds) => {
      let mut sig_env = SigEnv::default();
      // SML Definition (66), SML Definition (67)
      for sig_bind in sig_binds {
        let env = ck_sig_exp(bs, st, &sig_bind.exp)?;
        // allow shadowing.
        sig_env.insert(sig_bind.id.val, env_to_sig(env));
      }
      bs.sig_env.extend(sig_env);
    }
    // SML Definition (85), SML Definition (89)
    TopDec::FunDec(fun_binds) => {
      let mut fun_env = FunEnv::default();
      // SML Definition (86)
      for fun_bind in fun_binds {
        let sig_env = ck_sig_exp(bs, st, &fun_bind.sig_exp)?;
        let mut bs = bs.clone();
        bs.env.str_env.insert(fun_bind.str_id.val, sig_env.clone());
        let str_env = ck_str_exp(&bs, st, &fun_bind.str_exp)?;
        // TODO might not be right? a lot of stuff going on with ty names.
        let fun_sig = FunSig {
          input: env_to_sig(sig_env),
          output: env_to_sig(str_env),
        };
        // allow shadowing.
        fun_env.insert(fun_bind.fun_id.val, fun_sig);
      }
      bs.fun_env.extend(fun_env);
    }
  }
  st.subst.use_overloaded_defaults();
  Ok(())
}

/// SML Definition (65)
fn env_to_sig(env: Env) -> Sig {
  // TODO what about signature specs inside this sig?
  Sig {
    ty_names: env.ty_env.inner.values().copied().collect(),
    env,
  }
}

fn ck_str_exp(bs: &Basis, st: &mut State, str_exp: &Located<StrExp>) -> Result<Env> {
  match &str_exp.val {
    // SML Definition (50)
    StrExp::Struct(str_dec) => ck_str_dec(bs, st, str_dec),
    // SML Definition (51)
    StrExp::LongStrId(long) => match get_env(&bs.env, long)?.str_env.get(&long.last.val) {
      None => {
        let err = Error::Undefined(Item::Struct, long.last.val);
        Err(long.last.loc.wrap(err))
      }
      Some(env) => Ok(env.clone()),
    },
    // SML Definition (52), SML Definition (53)
    StrExp::Ascription(lhs, rhs, opaque) => {
      let env = ck_str_exp(bs, st, lhs)?;
      let mut sig = env_to_sig(ck_sig_exp(bs, st, rhs)?);
      let (env, _) = sig_match::ck(st, lhs.loc, env, &sig)?;
      if *opaque {
        let mut ty_rzn = TyRealization::default();
        for &old in sig.ty_names.iter() {
          let new = st.new_sym(str_exp.loc.wrap(old.name()));
          ty_rzn.insert_sym(old, new);
        }
        ty_rzn.get_env(&mut st.tys, &mut sig.env);
        Ok(sig.env)
      } else {
        Ok(env)
      }
    }
    // SML Definition (54)
    StrExp::FunctorApp(fun_id, arg) => match bs.fun_env.get(&fun_id.val) {
      None => Err(fun_id.loc.wrap(Error::Undefined(Item::Functor, fun_id.val))),
      Some(fun_sig) => {
        let arg_env = ck_str_exp(bs, st, arg)?;
        let (_, mut ty_rzn) = sig_match::ck(st, arg.loc, arg_env, &fun_sig.input)?;
        let mut ret = fun_sig.output.env.clone();
        for &old in fun_sig.output.ty_names.iter() {
          let new = st.new_sym(str_exp.loc.wrap(old.name()));
          ty_rzn.insert_sym(old, new);
        }
        ty_rzn.get_env(&mut st.tys, &mut ret);
        Ok(ret)
      }
    },
    // SML Definition (55)
    StrExp::Let(fst, snd) => {
      let env = ck_str_dec(bs, st, fst)?;
      let mut bs = bs.clone();
      bs.env.extend(env);
      ck_str_exp(&bs, st, snd)
    }
  }
}

fn ck_str_dec(bs: &Basis, st: &mut State, str_dec: &Located<StrDec>) -> Result<Env> {
  match &str_dec.val {
    // SML Definition (56)
    StrDec::Dec(dec) => dec::ck(&bs.to_cx(), st, dec),
    // SML Definition (57)
    StrDec::Structure(str_binds) => {
      let mut str_env = StrEnv::new();
      // SML Definition (61)
      for str_bind in str_binds {
        let env = ck_str_exp(bs, st, &str_bind.exp)?;
        // allow shadowing.
        str_env.insert(str_bind.id.val, env);
      }
      Ok(str_env.into())
    }
    // SML Definition (58)
    StrDec::Local(fst, snd) => {
      let env = ck_str_dec(bs, st, fst)?;
      let mut bs = bs.clone();
      bs.env.extend(env);
      ck_str_dec(&bs, st, snd)
    }
    // SML Definition (59), SML Definition (60)
    StrDec::Seq(str_decs) => {
      let mut bs = bs.clone();
      let mut ret = Env::default();
      for str_dec in str_decs {
        bs.env.extend(ret.clone());
        ret.extend(ck_str_dec(&bs, st, str_dec)?);
      }
      Ok(ret)
    }
  }
}

fn ck_sig_exp(bs: &Basis, st: &mut State, sig_exp: &Located<SigExp>) -> Result<Env> {
  match &sig_exp.val {
    // SML Definition (62)
    SigExp::Sig(spec) => ck_spec(bs, st, spec),
    // SML Definition (63)
    SigExp::SigId(sig_id) => match bs.sig_env.get(&sig_id.val) {
      None => Err(sig_id.loc.wrap(Error::Undefined(Item::Sig, sig_id.val))),
      // TODO I don't understand the check to see if the type names of the sig are disjoint from the
      // type names of the basis. It seems like this will _always_ be the case since when we process
      // a `signature` top dec, we add the type names of that sig to the basis. (Why do we do that?
      // I don't know). Is the whole "you may need to rename bound type names" thing made not
      // necessary by `Sym`, which is meant to be globally unique?
      Some(sig) => Ok(sig.env.clone()),
    },
    // SML Definition (64)
    SigExp::Where(_, _, _, _) => Err(sig_exp.loc.wrap(Error::Todo("`where`"))),
  }
}

fn ck_spec(bs: &Basis, st: &mut State, spec: &Located<Spec>) -> Result<Env> {
  match &spec.val {
    // SML Definition (68)
    Spec::Val(val_descs) => {
      let cx = bs.to_cx();
      let mut val_env = ValEnv::new();
      // SML Definition (79)
      for val_desc in val_descs {
        let ty = ty::ck(&cx, &st.tys, &val_desc.ty)?;
        // TODO generalize? closure?
        let val_info = ValInfo::val(TyScheme::mono(ty));
        env_ins(&mut val_env, val_desc.vid, val_info, Item::Val)?;
      }
      Ok(val_env.into())
    }
    // SML Definition (69), SML Definition (70)
    Spec::Type(ty_descs, equality) => {
      let mut ty_env = TyEnv::default();
      // SML Definition (80)
      for ty_desc in ty_descs {
        if let Some(tv) = ty_desc.ty_vars.first() {
          return Err(tv.loc.wrap(Error::Todo("type variables in spec")));
        }
        let sym = st.new_sym(ty_desc.ty_con);
        // TODO equality check
        env_ins(&mut ty_env.inner, ty_desc.ty_con, sym, Item::Ty)?;
        st.tys.insert(
          sym,
          TyInfo {
            ty_fcn: TyScheme::mono(Ty::Ctor(vec![], sym)),
            val_env: ValEnv::new(),
            equality: *equality,
          },
        );
      }
      Ok(ty_env.into())
    }
    // SML Definition (71)
    Spec::Datatype(dat_binds) => dec::ck_dat_binds(bs.to_cx(), st, dat_binds),
    // SML Definition (72)
    Spec::DatatypeCopy(ty_con, long) => dec::ck_dat_copy(&bs.to_cx(), &st.tys, *ty_con, long),
    // SML Definition (73)
    Spec::Exception(ex_descs) => {
      let cx = bs.to_cx();
      let mut val_env = ValEnv::new();
      // SML Definition (83)
      for ex_desc in ex_descs {
        let val_info = match &ex_desc.ty {
          None => ValInfo::exn(),
          Some(ty) => ValInfo::exn_fn(ty::ck(&cx, &st.tys, ty)?),
        };
        env_ins(&mut val_env, ex_desc.vid, val_info, Item::Val)?;
      }
      Ok(val_env.into())
    }
    // SML Definition (74)
    Spec::Structure(str_descs) => {
      let mut str_env = StrEnv::new();
      // SML Definition (84)
      for str_desc in str_descs {
        let env = ck_sig_exp(bs, st, &str_desc.exp)?;
        // allow shadowing.
        str_env.insert(str_desc.str_id.val, env);
      }
      Ok(str_env.into())
    }
    // SML Definition (75)
    Spec::Include(sig_exp) => ck_sig_exp(bs, st, sig_exp),
    // SML Definition (76), SML Definition (77)
    Spec::Seq(specs) => {
      let mut bs = bs.clone();
      let mut ret = Env::default();
      for spec in specs {
        bs.env.extend(ret.clone());
        let env = ck_spec(&bs, st, spec)?;
        ret.maybe_extend(env, spec.loc)?;
      }
      Ok(ret)
    }
    // SML Definition (78)
    Spec::Sharing(_, _) => Err(spec.loc.wrap(Error::Todo("`sharing`"))),
  }
}
