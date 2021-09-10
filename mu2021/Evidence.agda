
open import Data.Nat using (ℕ ; _≤_; _+_ ; _*_; zero ; suc)

-- Evidence vs Proof

data Bool : Set where
  true  : Bool
  false : Bool

record ⊤ : Set where
data   ⊥ : Set where

_<c_ : ℕ → ℕ → Bool
m       <c zero = false
zero    <c (suc n) = true
(suc m) <c (suc n) = m <c n

holds : Bool → Set
holds true  = ⊤
holds false = ⊥

data _<r_ : ℕ → ℕ → Set where
  z<rs : ∀ {m}   → zero <r (suc m)
  s<rs : ∀ {m n} → m <r n → suc m <r suc n

data Vector : (A : Set) → ℕ → Set where
  []  : ∀ {A}   → Vector A 0
  _∷_ : ∀ {A n} → A → Vector A n → Vector A (1 + n)

lookupc : ∀ {A n} → Vector A n → (m : ℕ) → holds (m <c n) → A
lookupc (x ∷ xs) zero m<cn = {!!}
lookupc (x ∷ xs) (suc m) m<cn = {!!}

lookupr : ∀ {A n} → Vector A n → (m : ℕ) → m <r n → A
lookupr xs m m<cn = {!!}

-- Equivalence

<c→<r : ∀ {m n} → holds (m <c n) → m <r n
<c→<r m<cn = {!!}

<r→<c : ∀ {m n} → m <r n → holds (m <c n)
<r→<c m<rn = {!!}

