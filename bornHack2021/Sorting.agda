
-- A hello world program.

open import Data.Bool using
  (Bool ; true ; false)
open import Data.Nat  using
  (ℕ ; zero ; suc ; _≤_ ; z≤n ; s≤s)
open import Data.Sum  using () renaming
  (_⊎_ to either ; inj₁ to left ; inj₂ to right)
open import Data.Product using (_,_) renaming
  (_×_ to _and_  ; proj₁ to fst ; proj₂ to snd)
open import Data.Unit using
  (⊤ ; tt)

infixr 5 _∷_

data List (A : Set) : Set where
  []  : List A
  _∷_ : (n : A) → (ns : List A) → List A

-- Examples

foldr : {A B : Set} → (f : A → B → B) → B → List A → B
foldr f b [] = b
foldr f b (a ∷ as) = f a (foldr f b as)

map : {A B : Set} → (f : A → B) → List A → List B
map f as = foldr (λ b bs → (f b) ∷ bs) [] as

-- More Warmup.

≤pred : ∀ {m n : ℕ} → (suc m ≤ suc n) → m ≤ n
≤pred sm≤sn = {!!}

≤suc : ∀ {m n : ℕ} → m ≤ n → suc m ≤ suc n
≤suc m≤n = {!!}

≤s : ∀ {m} → m ≤ suc m
≤s = {!!}

≤refl : ∀ {m} → m ≤ m
≤refl = {!!}

≤trans : ∀ {m n k} → m ≤ n → n ≤ k → m ≤ k
≤trans m≤n n≤k = {!!}

≤total : ∀ m n → either (m ≤ n) (n ≤ m)
≤total m n = {!!}

-- Insertion Sort

insert : (m : ℕ) → (ns : List ℕ) → List ℕ
insert m ns = {!!}

sort : (ns : List ℕ) → List ℕ
sort ns = {!!}

data ⊥ : Set where

_≤all_ : (m : ℕ) → (ns : List ℕ) → Set
m ≤all ns = {!!}

sorted : (ns : List ℕ) → Set
sorted ns = {!!}

insert-correct : (m : ℕ) → (ns : List ℕ) → sorted ns → sorted (insert m ns)
insert-correct m ns proof = {!!}

sort-correct : ∀ ns → sorted (sort ns)
sort-correct ns = {!!}

