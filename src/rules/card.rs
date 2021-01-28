use std::cmp::Ordering;
use std::marker::PhantomData;
use std::fmt::{Display, Formatter, Result as FmtResult};
use serde::{Serialize, Deserialize};
use rand::prelude::*;
use rand_distr::{Standard, Distribution};

pub type PropertyIx = usize;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
pub struct OrdProperty<C: Card>(pub PropertyIx, pub PhantomData<C>)
where
    Standard: Distribution<C>;

impl<C: Card> Distribution<OrdProperty<C>> for Standard
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> OrdProperty<C> {
        OrdProperty(rng.gen_range(0..C::num_ord_properties()), PhantomData)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
pub enum EqProperty<C: Card>
where
    Standard: Distribution<C>,
{
    Ord(OrdProperty<C>),
    Eq(PropertyIx),
}

impl<C: Card> Distribution<EqProperty<C>> for Standard
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> EqProperty<C> {
        let ords = C::num_ord_properties();
        let eqs = C::num_eq_properties();

        if rng.gen_bool(ords as f64 / (ords + eqs) as f64) {
            EqProperty::Ord(rng.gen::<OrdProperty<C>>())
        } else {
            EqProperty::Eq(rng.gen_range(0..eqs))
        }
    }
}

pub trait Card: Send + Eq + Clone
where
    Standard: Distribution<Self>,
{
    /// Get all unique cards.
    fn all_unique_cards() -> Vec<Self>;

    /// Get the number of unique cards.
    fn num_unique_cards() -> usize {
        Self::all_unique_cards().len()
    }

    /// Get a deck of cards.
    ///
    /// Must be deterministic between calls.
    fn get_deck() -> Vec<Self>;

    /// Get the size of a deck.
    fn deck_size() -> usize {
        Self::get_deck().len()
    }

    /// Get the number of properties that have equality.
    ///
    /// This includes properties that can be ordered. Must be at least one.
    fn num_eq_properties() -> usize;

    /// Compare properties that can only have equality checked.
    fn eq_prop(&self, other: &Self, property: EqProperty<Self>) -> bool;

    /// Get the number of properties that are ordered.
    ///
    /// Must be less than or equal to the number of properties that have
    /// equality. Must be at least one.
    fn num_ord_properties() -> usize;

    /// Compare a certain property between two cards.
    fn cmp_prop(&self, other: &Self, property: OrdProperty<Self>) -> Ordering;

    const DEFAULT_ORD: OrdProperty<Self>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Common52Card { pub suit: u8, pub number: u8 }

impl Distribution<Common52Card> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Common52Card {
        Common52Card {
            suit: rng.gen_range(0..4),
            number: rng.gen_range(0..13),
        }
    }
}

impl Display for Common52Card {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        const SUITS: [&str; 4] = ["♠", "♥", "♣", "♦"];
        f.write_fmt(format_args!("{}{}", SUITS[self.suit as usize], self.number))
    }
}

impl Card for Common52Card {
    fn all_unique_cards() -> Vec<Self> {
        Self::ALL_CARDS.to_vec()
    }

    fn num_unique_cards() -> usize {
        Self::NUM_CARDS
    }

    fn get_deck() -> Vec<Self> {
        Self::all_unique_cards()
    }

    fn deck_size() -> usize {
        Self::NUM_CARDS
    }

    fn num_eq_properties() -> usize {
        2
    }

    fn eq_prop(&self, other: &Self, property: EqProperty<Self>) -> bool {
        match property {
            EqProperty::Eq(0) => self.suit == other.suit,
            EqProperty::Eq(1) => self == other,
            EqProperty::Eq(_) => unreachable!("only two eqs for EqProperty<Common52Card>"),
            EqProperty::Ord(property) => self.cmp_prop(other, property) == Ordering::Equal,
        }
    }

    fn num_ord_properties() -> usize {
        2
    }

    fn cmp_prop(&self, other: &Self, OrdProperty(property, _): OrdProperty<Self>) -> Ordering {
        // property == 0 -> ace is least; else, ace -> greatest
        let lnum = if self.number == 0 {
            if property == 0 {
                0
            } else {
                13
            }
        } else {
            self.number
        };

        let rnum = if other.number == 0 {
            if property == 0 {
                0
            } else {
                13
            }
        } else {
            other.number
        };

        lnum.cmp(&rnum)
    }

    const DEFAULT_ORD: OrdProperty<Self> = OrdProperty(0, PhantomData);
}

impl Common52Card {
    pub const NUM_CARDS: usize = 52;
    pub const ALL_CARDS: [Self; Self::NUM_CARDS] = [
        Common52Card { suit: 0, number: 0 }, Common52Card { suit: 1, number: 0 },
        Common52Card { suit: 2, number: 0 }, Common52Card { suit: 3, number: 0 },
        Common52Card { suit: 0, number: 1 }, Common52Card { suit: 1, number: 1 },
        Common52Card { suit: 2, number: 1 }, Common52Card { suit: 3, number: 1 },
        Common52Card { suit: 0, number: 2 }, Common52Card { suit: 1, number: 2 },
        Common52Card { suit: 2, number: 2 }, Common52Card { suit: 3, number: 2 },
        Common52Card { suit: 0, number: 3 }, Common52Card { suit: 1, number: 3 },
        Common52Card { suit: 2, number: 3 }, Common52Card { suit: 3, number: 3 },
        Common52Card { suit: 0, number: 4 }, Common52Card { suit: 1, number: 4 },
        Common52Card { suit: 2, number: 4 }, Common52Card { suit: 3, number: 4 },
        Common52Card { suit: 0, number: 5 }, Common52Card { suit: 1, number: 5 },
        Common52Card { suit: 2, number: 5 }, Common52Card { suit: 3, number: 5 },
        Common52Card { suit: 0, number: 6 }, Common52Card { suit: 1, number: 6 },
        Common52Card { suit: 2, number: 6 }, Common52Card { suit: 3, number: 6 },
        Common52Card { suit: 0, number: 7 }, Common52Card { suit: 1, number: 7 },
        Common52Card { suit: 2, number: 7 }, Common52Card { suit: 3, number: 7 },
        Common52Card { suit: 0, number: 8 }, Common52Card { suit: 1, number: 8 },
        Common52Card { suit: 2, number: 8 }, Common52Card { suit: 3, number: 8 },
        Common52Card { suit: 0, number: 9 }, Common52Card { suit: 1, number: 9 },
        Common52Card { suit: 2, number: 9 }, Common52Card { suit: 3, number: 9 },
        Common52Card { suit: 0, number: 10 }, Common52Card { suit: 1, number: 10 },
        Common52Card { suit: 2, number: 10 }, Common52Card { suit: 3, number: 10 },
        Common52Card { suit: 0, number: 11 }, Common52Card { suit: 1, number: 11 },
        Common52Card { suit: 2, number: 11 }, Common52Card { suit: 3, number: 11 },
        Common52Card { suit: 0, number: 12 }, Common52Card { suit: 1, number: 12 },
        Common52Card { suit: 2, number: 12 }, Common52Card { suit: 3, number: 12 },
    ];
}

impl Display for OrdProperty<Common52Card> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "in order of value with ace {}", match self.0 {
            0 => "least",
            1 => "greatest",
            _ => unreachable!("only two orders for OrdProperty<Common52Card>"),
        })
    }
}

impl Display for EqProperty<Common52Card> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "when considering {}", match self {
            EqProperty::Eq(0) => "only suit",
            EqProperty::Eq(1) => "the whole card",
            EqProperty::Eq(_) => unreachable!("only two eqs for EqProperty<Common52Card>"),
            EqProperty::Ord(_) => "only value",
        })
    }
}
