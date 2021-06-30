
open import Data.Nat using (ℕ ; _≤_; _+_ ; _*_; zero ; suc)

data Bool : Set where
  true  : Bool
  false : Bool

infixr 20 _∷_

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

-- Warmup

map : {A B : Set} → (f : A → B) → List A → List B
map f xs = {!!}

filter : {A : Set} → (p : A → Bool) → List A → List A
filter p xs = {!!}

infix 10 _⊆_

data _⊆_ {A : Set} : List A → List A → Set where
  stop : [] ⊆ []
  keep : ∀ {x xs ys} → xs ⊆ ys → x ∷ xs ⊆ x ∷ ys
  drop : ∀ {xs y ys} → xs ⊆ ys →     xs ⊆ y ∷ ys

filter-⊆ : {A : Set} → (p : A → Bool) → (xs : List A) → filter p xs ⊆ xs
filter-⊆ p xs = {!!}

-- Evidence vs Computation.

record ⊤ : Set where
data   ⊥ : Set where

data Vector : (A : Set) → ℕ → Set where
  []  : ∀ {A}   → Vector A 0
  _∷_ : ∀ {A n} → A → Vector A n → Vector A (1 + n)

_<c_ : ℕ → ℕ → Bool
m       <c zero = false
zero    <c (suc n) = true
(suc m) <c (suc n) = m <c n

holds : Bool → Set
holds true  = ⊤
holds false = ⊥

lookupc : ∀ {A n} → Vector A n → (m : ℕ) → holds (m <c n) → A
lookupc xs m m<cn = {!!}

data _<r_ : ℕ → ℕ → Set where
  z<rs : ∀ {m}   → zero <r (suc m)
  s<rs : ∀ {m n} → m <r n → suc m <r suc n

lookupr : ∀ {A n} → Vector A n → (m : ℕ) → m <r n → A
lookupr xs m m<cn = {!!}

-- Equivalence

<c→<r : ∀ {m n} → holds (m <c n) → m <r n
<c→<r m<cn = {!!}

<r→<c : ∀ {m n} → m <r n → holds (m <c n)
<r→<c m<rn = {!!}

