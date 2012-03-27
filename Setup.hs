import Simple

import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook  = myConfHook,
                                              buildHook = Simple.defaultBuildHook }

myConfHook (pkg0, pbi) flags = do
    lbi <- confHook simpleUserHooks (pkg0, pbi) flags
    return lbi
