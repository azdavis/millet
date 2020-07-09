//! Check top-level declarations.

use crate::ast::{SigExp, Spec, StrDec, StrExp, TopDec};
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::util::env_ins;
use crate::statics::ck::{dec, ty};
use crate::statics::types::{
  Basis, Env, Error, FunEnv, Result, Sig, SigEnv, State, StrEnv, SymTyInfo, Ty, TyEnv, TyInfo,
  TyScheme, ValEnv, ValInfo,
};

pub fn ck(bs: &mut Basis, st: &mut State, top_dec: &Located<TopDec<StrRef>>) -> Result<()> {
  match &top_dec.val {
    TopDec::StrDec(str_dec) => {
      let env = ck_str_dec(bs, st, str_dec)?;
      bs.add_env(env);
    }
    TopDec::SigDec(sig_binds) => {
      let mut sig_env = SigEnv::new();
      for sig_bind in sig_binds {
        let env = ck_sig_exp(bs, st, &sig_bind.exp)?;
        sig_env.insert(sig_bind.id.val, env_to_sig(bs, env));
      }
      bs.add_sig_env(sig_env);
    }
    TopDec::FunDec(fun_binds) => {
      let fun_env = FunEnv::new();
      if let Some(fun_bind) = fun_binds.first() {
        return Err(fun_bind.fun_id.loc.wrap(Error::Todo));
      }
      bs.add_fun_env(fun_env);
    }
  }
  Ok(())
}

fn env_to_sig(bs: &Basis, env: Env) -> Sig {
  let ty_names = env.ty_names().difference(&bs.ty_names).copied().collect();
  Sig { env, ty_names }
}

fn ck_str_dec(bs: &Basis, st: &mut State, str_dec: &Located<StrDec<StrRef>>) -> Result<Env> {
  match &str_dec.val {
    StrDec::Dec(dec) => dec::ck(&bs.to_cx(), st, dec),
    StrDec::Structure(str_binds) => {
      let mut bs = bs.clone();
      let mut str_env = StrEnv::new();
      for str_bind in str_binds {
        let env = ck_str_exp(&bs, st, &str_bind.exp)?;
        bs.ty_names.extend(env.ty_names());
        str_env.insert(str_bind.id.val, env);
      }
      Ok(str_env.into())
    }
    StrDec::Local(fst, snd) => {
      let env = ck_str_dec(bs, st, fst)?;
      let mut bs = bs.clone();
      bs.add_env(env);
      ck_str_dec(&bs, st, snd)
    }
    StrDec::Seq(str_decs) => {
      // TODO clone in loop - expensive?
      let mut bs = bs.clone();
      let mut ret = Env::default();
      for str_dec in str_decs {
        bs.add_env(ret.clone());
        ret.extend(ck_str_dec(&bs, st, str_dec)?);
      }
      Ok(ret)
    }
  }
}

fn ck_str_exp(bs: &Basis, st: &mut State, str_exp: &Located<StrExp<StrRef>>) -> Result<Env> {
  match &str_exp.val {
    StrExp::Struct(_) => {
      //
      Err(str_exp.loc.wrap(Error::Todo))
    }
    StrExp::LongStrId(_) => {
      //
      Err(str_exp.loc.wrap(Error::Todo))
    }
    StrExp::Ascription(_, _, _) => {
      //
      Err(str_exp.loc.wrap(Error::Todo))
    }
    StrExp::FunctorApp(_, _) => {
      //
      Err(str_exp.loc.wrap(Error::Todo))
    }
    StrExp::Let(fst, snd) => {
      let env = ck_str_dec(bs, st, fst)?;
      let mut bs = bs.clone();
      bs.add_env(env);
      ck_str_exp(&bs, st, snd)
    }
  }
}

fn ck_sig_exp(bs: &Basis, st: &mut State, sig_exp: &Located<SigExp<StrRef>>) -> Result<Env> {
  match &sig_exp.val {
    SigExp::Sig(spec) => ck_spec(bs, st, spec),
    SigExp::SigId(_) => {
      //
      Err(sig_exp.loc.wrap(Error::Todo))
    }
    SigExp::Where(_, _, _, _) => {
      //
      Err(sig_exp.loc.wrap(Error::Todo))
    }
  }
}

fn ck_spec(bs: &Basis, st: &mut State, spec: &Located<Spec<StrRef>>) -> Result<Env> {
  match &spec.val {
    Spec::Val(val_descs) => {
      let cx = bs.to_cx();
      let mut val_env = ValEnv::new();
      for val_desc in val_descs {
        let ty = ty::ck(&cx, st, &val_desc.ty)?;
        // TODO generalize? closure?
        env_ins(&mut val_env, val_desc.vid, ValInfo::val(TyScheme::mono(ty)))?;
      }
      Ok(val_env.into())
    }
    Spec::Type(ty_descs, _) => {
      let mut ty_env = TyEnv::default();
      for ty_desc in ty_descs {
        if let Some(tv) = ty_desc.ty_vars.first() {
          return Err(tv.loc.wrap(Error::Todo));
        }
        let sym = st.new_sym(ty_desc.ty_con);
        // TODO equality check
        env_ins(&mut ty_env.inner, ty_desc.ty_con, TyInfo::Sym(sym))?;
        st.sym_tys.insert(
          sym,
          SymTyInfo {
            ty_fcn: TyScheme::mono(Ty::Ctor(vec![], sym)),
            val_env: ValEnv::new(),
          },
        );
      }
      Ok(ty_env.into())
    }
    Spec::Datatype(dat_binds) => dec::ck_dat_binds(bs.to_cx(), st, dat_binds),
    Spec::DatatypeCopy(ty_con, long) => dec::ck_dat_copy(&bs.to_cx(), st, *ty_con, long),
    Spec::Exception(ex_descs) => {
      let cx = bs.to_cx();
      let mut val_env = ValEnv::new();
      for ex_desc in ex_descs {
        let val_info = match &ex_desc.ty {
          None => ValInfo::exn(),
          Some(ty) => ValInfo::exn_fn(ty::ck(&cx, st, ty)?),
        };
        env_ins(&mut val_env, ex_desc.vid, val_info)?;
      }
      Ok(val_env.into())
    }
    Spec::Structure(_) => {
      //
      Err(spec.loc.wrap(Error::Todo))
    }
    Spec::Include(_) => {
      //
      Err(spec.loc.wrap(Error::Todo))
    }
    Spec::Seq(specs) => {
      let mut ret = Env::default();
      for spec in specs {
        let env = ck_spec(bs, st, spec)?;
        ret.maybe_extend(env, spec.loc)?;
      }
      Err(spec.loc.wrap(Error::Todo))
    }
    Spec::Sharing(_, _) => {
      //
      Err(spec.loc.wrap(Error::Todo))
    }
  }
}
