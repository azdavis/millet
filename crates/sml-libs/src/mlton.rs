//! [`MLton` libraries](http://mlton.org/MLtonStructure).

use crate::files;

/// The files.
pub const FILES: &[(&str, &str)] = files![
  "mlton/MLtonArray.sml",
  "mlton/MLtonIO.sml",
  "mlton/MLtonBinIO.sml",
  "mlton/MLtonTextIO.sml",
  "mlton/MLtonCont.sml",
  "mlton/MLtonExn.sml",
  "mlton/MLtonFinalizable.sml",
  "mlton/MLtonGC.sml",
  "mlton/MLtonIntInf.sml",
  "mlton/MLtonItimer.sml",
  "mlton/MLtonMonoArray.sml",
  "mlton/MLtonMonoVector.sml",
  "mlton/MLtonPlatform.sml",
  "mlton/MLtonPointer.sml",
  "mlton/MLtonProcEnv.sml",
  "mlton/MLtonProcess.sml",
  "mlton/MLtonProfile.sml",
  "mlton/MLtonRandom.sml",
  "mlton/MLtonReal.sml",
  "mlton/MLtonRlimit.sml",
  "mlton/MLtonRusage.sml",
  "mlton/MLtonThread.sml",
  "mlton/MLtonSignal.sml",
  "mlton/MLtonSyslog.sml",
  "mlton/MLtonVector.sml",
  "mlton/MLtonWeak.sml",
  "mlton/MLtonWord.sml",
  "mlton/MLtonWorld.sml",
  "mlton/MLton.sml",
];
