module FamiliesExtra where
open import Basics using (⋆)
open import Nat
open import Bool
open import CurryHoward using (_∧_; _,_)
open import Families


open import Relation.Binary.PropositionalEquality as PropEq
  using (_≡_; refl; sym; cong; cong₂)
open PropEq.≡-Reasoning

-- Some useful utilities to illustrate simple induction proofs
plusZero : ∀ m ->    m ≡ m + 0 
plusZero zero     =  refl
plusZero (suc m)  =  cong suc (plusZero m)

plusSuc : ∀ m n ->   suc (m + n) ≡ m + suc n
plusSuc zero     n = refl 
plusSuc (suc m)  n = cong suc (plusSuc m n)

-- Equational reasoning:
plusComm : ∀ (m n : Nat) ->     m + n  ≡  n + m
plusComm zero      n = plusZero n
plusComm (suc m)   n = 
  begin
    suc m + n
  ≡⟨ refl ⟩
    suc (m + n)
  ≡⟨ cong suc (plusComm m n) ⟩
    suc (n + m)
  ≡⟨ plusSuc n m ⟩
    n + suc m
  ∎












-- Proving vector has another type requires a type change (coerce)
coerce : forall {A} n m -> (n ≡ m) -> Vec A n -> Vec A m
coerce n .n refl v = v

cat' : {A : ⋆}(n m : Nat) -> Vec A n -> Vec A m -> Vec A (m + n)
cat' {A} n m xs ys = coerce  (n + m) (m + n) (plusComm n m) 
                             (cat A n m xs ys)
