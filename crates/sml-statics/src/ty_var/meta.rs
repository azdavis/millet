//! Meta type variables, as constructed by the type inference engine.

/// Generated, and to be solved for a real type, by the inference algorithm.
///
/// Should eventually be solved in a `Subst`, but before that, it may be "restricted" by the
/// `Subst` without yet being fully solved to a type.
///
/// Internally contains a "rank" to know when it should be generalizable; see "Efficient ML Type
/// Inference Using Ranked Type Variables" (doi:10.1145/1292535.1292538)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct MetaTyVar {
  id: u32,
  rank: MetaTyVarRank,
}

impl MetaTyVar {
  pub(crate) fn rank_lt(self, other: Self) -> bool {
    self.rank < other.rank
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum MetaTyVarRank {
  Finite(u16),
  Infinite,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Generalizable {
  Sometimes,
  Always,
}

#[derive(Debug, Default)]
pub(crate) struct MetaTyVarGen {
  id: u32,
  rank: u16,
}

impl MetaTyVarGen {
  pub(crate) fn gen(&mut self, g: Generalizable) -> MetaTyVar {
    let ret = MetaTyVar {
      id: self.id,
      rank: match g {
        Generalizable::Sometimes => MetaTyVarRank::Finite(self.rank),
        Generalizable::Always => MetaTyVarRank::Infinite,
      },
    };
    self.id += 1;
    ret
  }

  pub(crate) fn inc_rank(&mut self) {
    self.rank += 1;
  }

  pub(crate) fn dec_rank(&mut self) {
    self.rank -= 1;
  }

  pub(crate) fn generalizer(&self) -> MetaTyVarGeneralizer {
    MetaTyVarGeneralizer { rank: self.rank }
  }

  pub(crate) fn gen_same_rank(&mut self, mv: MetaTyVar) -> MetaTyVar {
    let ret = MetaTyVar { id: self.id, rank: mv.rank };
    self.id += 1;
    ret
  }
}

#[derive(Debug)]
pub(crate) struct MetaTyVarGeneralizer {
  rank: u16,
}

impl MetaTyVarGeneralizer {
  /// Returns in O(1) time.
  pub(crate) fn is_generalizable(&self, mv: MetaTyVar) -> bool {
    match mv.rank {
      MetaTyVarRank::Finite(r) => r > self.rank,
      MetaTyVarRank::Infinite => true,
    }
  }
}
