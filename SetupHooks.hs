{-# LANGUAGE DisambiguateRecordFields #-}

module SetupHooks where
import Distribution.Simple.SetupHooks
import Distribution.Utils.Path qualified as P
import System.Exit
import System.Process

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks {
    configureHooks = noConfigureHooks { preConfComponentHook = Just preconfigure }
  }

preconfigure :: PreConfComponentHook
preconfigure input@(PreConfComponentInputs {component}) = do
  P.AbsolutePath libDir <- P.absoluteWorkingDir (Just $ P.makeSymbolicPath "matcher/target/release")

  

  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode
    ((shell "cargo build --release") { cwd = Just (P.interpretSymbolicPathCWD (P.makeSymbolicPath "matcher"))})
    ""

  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure e -> do
      putStrLn "Failed to build Rust matcher library"
      putStrLn stderr
      putStrLn stdout
      exitWith exitCode

  if componentName component == CLibName LMainLibName
    then pure $
      (noPreConfComponentOutputs input) {
        componentDiff = buildInfoComponentDiff (CLibName LMainLibName)
          emptyBuildInfo {extraLibDirs = [libDir]}
      }
    else pure $ noPreConfComponentOutputs input
