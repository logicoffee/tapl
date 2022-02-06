import Test.Tasty
import Test.Tasty.HUnit
import Untyped

main :: IO ()
main = do
  print $ eval1 [] t1
  defaultMain tests
  where
    t1 = TmApp (TmAbs (TmApp (TmApp (TmVar 1) (TmVar 0)) (TmVar 2))) (TmAbs (TmVar 0))

tests :: TestTree
tests = testGroup "Tests" [untyped]

untyped :: TestTree
untyped = testGroup "Untyped" [termShiftTest, termSubstTest, eval1Test, evalBigStepTest]

-- 演習 6.2.2
termShiftTest :: TestTree
termShiftTest =
  testGroup
    "termShift"
    [ testCase "(1)" $ termShift 2 lhs1 @?= rhs1,
      testCase "(2)" $ termShift 2 lhs2 @?= rhs2
    ]
  where
    -- λ. λ. 1 (0 2)
    lhs1 = TmAbs (TmAbs (TmApp (TmVar 1) (TmApp (TmVar 0) (TmVar 2))))
    -- λ. λ. 1 (0 4)
    rhs1 = TmAbs (TmAbs (TmApp (TmVar 1) (TmApp (TmVar 0) (TmVar 4))))
    -- λ. 0 1 (λ. 0 1 2)
    lhs2 = TmAbs (TmApp (TmApp (TmVar 0) (TmVar 1)) (TmAbs (TmApp (TmApp (TmVar 0) (TmVar 1)) (TmVar 2))))
    -- λ. 0 3 (λ. 0 1 4)
    rhs2 = TmAbs (TmApp (TmApp (TmVar 0) (TmVar 3)) (TmAbs (TmApp (TmApp (TmVar 0) (TmVar 1)) (TmVar 4))))

termSubstTest :: TestTree
termSubstTest =
  testGroup
    "termSubst"
    [ testCase "[0 -> 1] 0 --> 1" $ termSubst 0 (TmVar 1) (TmVar 0) @?= TmVar 1,
      testCase "[0 -> 1] λ. 0 --> λ. 0" $ termSubst 0 (TmVar 1) (TmAbs (TmVar 0)) @?= TmAbs (TmVar 0),
      testCase "[0 -> 1] λ. 1 --> λ. 2" $ termSubst 0 (TmVar 1) (TmAbs (TmVar 1)) @?= TmAbs (TmVar 2)
    ]

eval1Test :: TestTree
eval1Test =
  testGroup
    "eval1"
    [ testCase "(λ. 0) (λ. 0) --> λ. 0" $ eval1 ctx (TmApp (TmAbs (TmVar 0)) (TmAbs (TmVar 0))) @?= Just (TmAbs (TmVar 0)),
      testCase "(λ. 1) (λ. 0) --> 0" $ eval1 ctx (TmApp (TmAbs (TmVar 1)) (TmAbs (TmVar 0))) @?= Just (TmVar 0),
      testCase "p61" $ eval1 ctx t1 @?= Just t2
    ]
  where
    ctx = []
    t1 = TmApp (TmAbs (TmApp (TmApp (TmVar 1) (TmVar 0)) (TmVar 2))) (TmAbs (TmVar 0))
    t2 = TmApp (TmApp (TmVar 0) (TmAbs (TmVar 0))) (TmVar 1)

evalBigStepTest :: TestTree
evalBigStepTest =
  testGroup
    "evalBigStep"
    [ testCase "(λ. 0) (λ. 0) --> λ. 0" $ evalBigStep ctx (TmApp (TmAbs (TmVar 0)) (TmAbs (TmVar 0))) @?= TmAbs (TmVar 0),
      testCase "(λ. 1) (λ. 0) --> 0" $ evalBigStep ctx (TmApp (TmAbs (TmVar 1)) (TmAbs (TmVar 0))) @?= TmVar 0,
      testCase "p61" $ evalBigStep ctx t1 @?= t2
    ]
  where
    ctx = []
    t1 = TmApp (TmAbs (TmApp (TmApp (TmVar 1) (TmVar 0)) (TmVar 2))) (TmAbs (TmVar 0))
    t2 = TmApp (TmApp (TmVar 0) (TmAbs (TmVar 0))) (TmVar 1)
