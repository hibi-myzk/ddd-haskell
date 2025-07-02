# F# to Haskell Conversion: Domain Modeling Made Functional

This project demonstrates the conversion of a Domain-Driven Design (DDD) order-taking system from F# to Haskell, based on the book "Domain Modeling Made Functional" by Scott Wlaschin.

## Overview

The original F# codebase implements a complete order-taking workflow with:
- Domain-driven design principles
- Type-safe constrained types
- Functional error handling with Result types
- Async workflows
- Comprehensive validation and business logic

This Haskell conversion maintains the same domain modeling principles while leveraging Haskell's powerful type system and functional programming features.

## Project Structure

```
haskell-order-taking/
├── src/
│   └── OrderTaking/
│       ├── Result.hs                    # Error handling utilities (Either-based)
│       ├── Common/
│       │   ├── ConstrainedTypes.hs     # Validation utilities
│       │   ├── SimpleTypes.hs          # Basic domain types with smart constructors
│       │   └── CompoundTypes.hs        # Composite domain types
│       └── PlaceOrder/
│           ├── PublicTypes.hs          # Public API types
│           ├── InternalTypes.hs        # Internal workflow types
│           ├── Implementation.hs       # Core business logic (stub)
│           └── Pricing.hs              # Pricing logic
├── app/
│   └── Main.hs                         # Demo application
├── order-taking.cabal                  # Project configuration
└── README.md                           # This file
```

## Key Conversion Mappings

### Type System Conversions

| F# Construct | Haskell Equivalent | Example |
|--------------|-------------------|---------|
| Single-case DU | `newtype` | `type String50 = String50 String` |
| Multi-case DU | `data` types | `data VipStatus = Normal \| Vip` |
| Records | `data` with record syntax | `data PersonalName = PersonalName { firstName :: String50, ... }` |
| Option types | `Maybe` | `Maybe String50` |
| Result types | `Either` | `Either ValidationError String50` |

### Error Handling

| F# | Haskell |
|----|---------|
| `Result<'Success, 'Failure>` | `Either Error Success` |
| `AsyncResult<'Success, 'Failure>` | `IO (Either Error Success)` |
| `result { ... }` computation expression | `do` notation with `Either` monad |

### Module Organization

| F# Module | Haskell Module | Purpose |
|-----------|----------------|---------|
| `OrderTaking.Common.SimpleTypes` | `OrderTaking.Common.SimpleTypes` | Constrained primitive types |
| `OrderTaking.Common.CompoundTypes` | `OrderTaking.Common.CompoundTypes` | Composite domain types |
| `OrderTaking.PlaceOrder.PublicTypes` | `OrderTaking.PlaceOrder.PublicTypes` | Public workflow API |
| `OrderTaking.PlaceOrder.InternalTypes` | `OrderTaking.PlaceOrder.InternalTypes` | Internal workflow types |

## Domain Types

### Constrained Types
All domain types use smart constructors for validation:

```haskell
-- String constrained to 50 characters
newtype String50 = String50 String

createString50 :: String -> String -> Either ValidationError String50
createString50 fieldName str
  | null str = Left $ ValidationError $ fieldName ++ " must not be empty"
  | length str > 50 = Left $ ValidationError $ fieldName ++ " must not exceed 50 characters"
  | otherwise = Right $ String50 str
```

### Product Codes
```haskell
data ProductCode = Widget WidgetCode | Gizmo GizmoCode

-- Widget codes: W + 4 digits (e.g., "W1234")
-- Gizmo codes: G + 3 digits (e.g., "G123")
```

### Order Quantities
```haskell
data OrderQuantity = Unit UnitQuantity | Kilogram KilogramQuantity

-- Units: 1-1000 (for widgets)
-- Kilograms: 0.05-100.00 (for gizmos)
```

## Key Features Implemented

### ✅ Completed
- [x] Core domain types with smart constructors
- [x] Validation utilities and constrained types
- [x] Error handling with Either-based Result types
- [x] Basic type structure for all workflow components
- [x] Async result utilities
- [x] Pattern matching for business rules

### 🚧 Partially Implemented
- [x] Basic pricing logic structure
- [x] Workflow type signatures
- [ ] Complete validation implementation
- [ ] Full pricing calculations
- [ ] Shipping logic
- [ ] Event creation

### 📋 TODO (Full Implementation)
- [ ] Complete validation workflow
- [ ] Pricing calculations with promotions
- [ ] Shipping cost calculations
- [ ] Order acknowledgment generation
- [ ] Event sourcing implementation
- [ ] Integration tests
- [ ] Property-based testing with QuickCheck

## Running the Demo

```bash
cd haskell-order-taking
cabal build
cabal run order-taking-exe
```

The demo application shows:
- Creating constrained types with validation
- Error handling with smart constructors
- Basic domain type operations

## Key Haskell Advantages

1. **Stronger Type System**: Haskell's type system provides even more compile-time guarantees
2. **Purity by Default**: All side effects are explicit in the type system
3. **Lazy Evaluation**: Efficient handling of large data structures
4. **Type Classes**: More flexible polymorphism than F# interfaces
5. **Property-Based Testing**: QuickCheck integration for comprehensive testing

## F# vs Haskell Comparison

| Aspect | F# | Haskell |
|--------|----|---------| 
| Type Inference | Excellent | Excellent |
| Pattern Matching | Excellent | Excellent |
| Immutability | Default | Enforced |
| Side Effects | Explicit with async | Explicit with IO monad |
| Error Handling | Result types | Either/Maybe types |
| Syntax | ML-family | Unique, mathematical |
| Ecosystem | .NET integration | Pure functional libraries |

## Design Patterns Converted

1. **Smart Constructors**: F# module functions → Haskell smart constructors
2. **Validation**: F# Result workflows → Haskell Either monads
3. **Pipeline Composition**: F# `|>` operator → Haskell function composition
4. **Type-Driven Development**: Both languages excel at this approach
5. **Domain Modeling**: Algebraic data types work similarly in both languages

## Learning Outcomes

This conversion demonstrates:
- How functional domain modeling translates between languages
- The power of algebraic data types for domain modeling
- Type-safe error handling patterns
- The importance of making illegal states unrepresentable
- Functional composition and pipeline thinking

## Next Steps

To complete the full implementation:
1. Implement the complete validation logic from F# Implementation.fs
2. Add comprehensive pricing calculations
3. Implement shipping and acknowledgment workflows
4. Add property-based tests
5. Create integration examples
6. Add performance benchmarks

## References

- [Domain Modeling Made Functional](https://pragprog.com/titles/swdddf/domain-modeling-made-functional/) by Scott Wlaschin
- [F# Original Source Code](../src/OrderTaking/)
- [Haskell Documentation](https://www.haskell.org/documentation/)
- [Real World Haskell](http://book.realworldhaskell.org/)

---

This conversion showcases how functional domain modeling principles transcend specific languages, while highlighting the unique strengths each language brings to functional programming and domain-driven design.
