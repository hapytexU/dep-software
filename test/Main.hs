{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- cabal v2-test --test-show-details=direct
-- cabal v2-test --test-show-details=direct --test-options="--color --format=progress"

main :: IO ExitCode
main = runAllLiquid

runAllLiquid :: IO ExitCode
runAllLiquid = mconcat <$> mapM runLiquid orderedSrcFiles
