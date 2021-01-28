use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::hint::unreachable_unchecked;
use std::collections::HashMap;
use serde::{Serialize, Deserialize, Serializer, Deserializer, ser::*};
use rand::prelude::*;
use rand_distr::Standard;

pub mod card;
pub mod dist;
pub use card::*;
pub use dist::*;

pub type Score = i64;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, PartialOrd, Ord)]
pub enum RuleNecessity {
    Mandatory,
    PlayOnce,
    Optional,
}

impl Display for RuleNecessity {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            RuleNecessity::Optional => "Optionally",
            RuleNecessity::PlayOnce => "At most once this stage",
            RuleNecessity::Mandatory => "At the beginning of this stage",
        })
    }
}

impl Distribution<RuleNecessity> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> RuleNecessity {
        match rng.gen_range(0..3) {
            0 => RuleNecessity::Optional,
            1 => RuleNecessity::PlayOnce,
            2 => RuleNecessity::Mandatory,
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum FakeOrdering {
    Less,
    Equal,
    Greater,
}

impl From<Ordering> for FakeOrdering {
    fn from(ord: Ordering) -> Self {
        match ord {
            Ordering::Less => FakeOrdering::Less,
            Ordering::Equal => FakeOrdering::Equal,
            Ordering::Greater => FakeOrdering::Greater,
        }
    }
}

impl From<FakeOrdering> for Ordering {
    fn from(ord: FakeOrdering) -> Self {
        match ord {
            FakeOrdering::Less => Ordering::Less,
            FakeOrdering::Equal => Ordering::Equal,
            FakeOrdering::Greater => Ordering::Greater,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct NumberOrdering {
    pub inverted: bool,
    pub ordering: Ordering,
}

impl Display for NumberOrdering {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(if self.inverted {
            match self.ordering {
                Ordering::Less => "at least",
                Ordering::Equal => "not equal to",
                Ordering::Greater => "at most",
            }
        } else {
            match self.ordering {
                Ordering::Less => "less than",
                Ordering::Equal => "equal to",
                Ordering::Greater => "greater than",
            }
        })
    }
}

impl Serialize for NumberOrdering {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut nord = serializer.serialize_struct("NumberOrdering", 2)?;
        nord.serialize_field("inverted", &self.inverted)?;
        nord.serialize_field::<FakeOrdering>("ordering", &self.ordering.into())?;
        nord.end()
    }
}

impl<'de> Deserialize<'de> for NumberOrdering {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct NumberOrdering {
            pub inverted: bool,
            pub ordering: FakeOrdering,
        }

        let NumberOrdering { inverted, ordering } = NumberOrdering::deserialize(deserializer)?;
        Ok(super::NumberOrdering { inverted, ordering: ordering.into() })
    }
}

impl Distribution<NumberOrdering> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> NumberOrdering {
        NumberOrdering {
            inverted: self.sample(rng),
            ordering: match rng.gen_range(0..3) {
                0 => Ordering::Equal,
                1 => Ordering::Less,
                2 => Ordering::Greater,
                //_ => unsafe { unreachable_unchecked() },
                _ => unreachable!(),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct CardOrdering<C: Card>(pub NumberOrdering, pub OrdProperty<C>)
where
    Standard: Distribution<C>;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum CardLocation {
    SpecificHand(usize),
    MyHand,
    Table(usize),
}

impl Display for CardLocation {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            CardLocation::SpecificHand(i) => write!(f, "player {}'s hand", i + 1),
            CardLocation::MyHand => f.write_str("my hand"),
            CardLocation::Table(i) => write!(f, "card pile {}", i + 1),
        }
    }
}

impl CardLocation {
    pub fn get_all(players: usize, tables: usize) -> Vec<Self> {
        let mut locs = vec![CardLocation::MyHand];
        locs.reserve_exact(players + tables);

        for player in 0..players {
            locs.push(CardLocation::SpecificHand(player));
        }

        for table in 0..tables {
            locs.push(CardLocation::Table(table));
        }

        locs.shrink_to_fit();
        locs
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum TokenLocation {
    SpecificBet { player: usize, stash: bool },
    MyBet,
    AllOthersBets,
}

impl Display for TokenLocation {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            TokenLocation::SpecificBet { player, stash: true } =>
                write!(f, "player {}'s stash", player + 1),
            TokenLocation::SpecificBet { player, stash: false } =>
                write!(f, "player {}'s bet", player + 1),
            TokenLocation::MyBet => f.write_str("my bet"),
            TokenLocation::AllOthersBets => f.write_str("everyone else's bet"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum HaveCondition<C: Card>
where
    Standard: Distribution<C>,
{
    PropertyOverlap(EqProperty<C>),
    PropertyExclusive(EqProperty<C>),
}

impl<C: Card> Display for HaveCondition<C>
where
    Standard: Distribution<C>,
    EqProperty<C>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            HaveCondition::PropertyOverlap(p) =>
                write!(f, "{}, where cards can fulfill multiple conditions", p),
            HaveCondition::PropertyExclusive(p) => write!(f, "{}, with one card per condition", p),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum EmptyCardLocBehavior {
    Tie,
    Die,
    End,
    Win(usize),
    RespawnAndReshuffle,
    TakeAndReshuffle { places: Vec<CardLocation>, fail: Box<EmptyCardLocBehavior> },
}

impl Display for EmptyCardLocBehavior {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            EmptyCardLocBehavior::Tie =>
                f.write_str("the game ends with a tie between remaining players"),
            EmptyCardLocBehavior::Die => f.write_str("the game 'crashes', something went wrong"),
            EmptyCardLocBehavior::End => f.write_str("the game ends, and scores are compared"),
            EmptyCardLocBehavior::Win(i) => write!(f, "player {} wins the game", i + 1),
            EmptyCardLocBehavior::RespawnAndReshuffle =>
                f.write_str("the pile is respawned with a full, shuffled deck"),
            EmptyCardLocBehavior::TakeAndReshuffle { places, fail } => {
                f.write_str("cards are taken from ")?;
                display_list(f, &places, |_| unreachable!(
                    "places to take from in EmptyCardLocBehavior::TakeAndReshuffle should never \
                     be empty",
                ))?;
                write!(f, ", but if those are empty, then {}", fail)
            },
        }
    }
}

/// A ranker that gives the "rank" (score) of a combination of cards.
///
/// This stores the rank of a combination of cards as a `Score`. An example of
/// the use of this is the hands of poker: different hands have better "score",
/// and thus it's necessary to check said hand's score against other hands to
/// see which player played a better hand.
///
/// Every combination must be sorted by the default card comparison.
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Ranker<C: Card>(pub Vec<(Vec<C>, Score)>)
where
    Standard: Distribution<C>;

impl<C: Card + Display> Display for Ranker<C>
where
    Standard: Distribution<C>,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if self.0.is_empty() {
            f.write_str("There are no special card rankings.\n")
        } else {
            for (cards, score) in &self.0 {
                write!(f, "For {:+} score: ", score)?;
                display_list(f, &cards, |_| unreachable!())?;
                f.write_str(".\n")?;
            }

            Ok(())
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum Antecedent<C: Card>
where
    Standard: Distribution<C>,
{
    Unconditional,
    Tokens(TokenLocation, NumberOrdering, TokenLocation),
    Play(Vec<CardLocation>, CardOrdering<C>, Vec<CardLocation>),   
    Sum(Vec<CardLocation>, NumberOrdering, Vec<CardLocation>), 
    Have(Vec<(HaveCondition<C>, Vec<C>)>),
    Draw(usize, CardLocation, bool),
    Show(CardOrdering<C>, CardLocation),
}

impl<C: Card + Display> Display for Antecedent<C>
where
    Standard: Distribution<C>,
    EqProperty<C>: Display,
    OrdProperty<C>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Antecedent::Unconditional => f.write_str("for any reason"),
            Antecedent::Tokens(tokl, ord, tokr) => write!(
                    f,
                    "the amount of tokens in {} is {} the amount of tokens in {}",
                    tokl,
                    ord,
                    tokr,
                ),
            Antecedent::Play(cardsl, CardOrdering(ord, prop), cardsr) => {
                f.write_str("the score of the best play in ")?;
                display_list_heads(
                    f,
                    cardsl,
                    |_| unreachable!(),
                    |_| Ok(()),
                    |f| Ok(()),
                    |_| Ok(()),
                )?;

                if cardsl.len() > 1 {
                    write!(f, " combined is {} the best play in ", ord)?;
                } else {
                    write!(f, " is {} the best play in ", ord)?;
                }

                display_list_heads(
                    f,
                    cardsr,
                    |_| unreachable!(),
                    |_| Ok(()),
                    |_| Ok(()),
                    |_| Ok(()),
                )?;

                if cardsr.len() > 1 {
                    write!(
                        f,
                        " combined (with {} as fallback, see docs for when no plays)",
                        prop,
                    )
                } else {
                    write!(f, " (with {} as fallback, see docs for when no plays)", prop)
                }
            },
            Antecedent::Sum(cardsl, ord, cardsr) => {
                display_list_heads(
                    f,
                    cardsl,
                    |f| f.write_str("0"),
                    |f| f.write_str("the number of cards in "),
                    |f| f.write_str("the number of cards in "),
                    |f| f.write_str("the number of cards in "),
                )?;

                if cardsl.len() > 1 {
                    write!(f, " combined is {} the best play in ", ord)?;
                } else {
                    write!(f, " is {} the best play in ", ord)?;
                }

                display_list_heads(
                    f,
                    cardsr,
                    |f| f.write_str("0"),
                    |f| f.write_str("the number of cards in "),
                    |f| f.write_str("the number of cards in "),
                    |f| f.write_str("the number of cards in "),
                )?;

                if cardsr.len() > 1 {
                    f.write_str(" combined")
                } else {
                    Ok(())
                }
            },
            Antecedent::Have(haves) => {
                for (have, cards) in haves {
                    f.write_str("\n  * has card")?;

                    display_list_heads(
                        f,
                        cards,
                        |_| unreachable!(),
                        |f| f.write_str(" "),
                        |f| f.write_str("s "),
                        |f| f.write_str("s "),
                    )?;

                    write!(f, " {}", have)?;
                }

                Ok(())
            },
            Antecedent::Draw(num, cloc, hidden) => write!(
                f,
                "draw {} card{} from {}, {}",
                num,
                if *num == 1 {
                    ""
                } else {
                    "s"
                },
                cloc,
                if *hidden {
                    "without revealing"
                } else {
                    "face up"
                }),
            Antecedent::Show(CardOrdering(ord, prop), cloc) => write!(
                f,
                "show a set of cards such that all are {} a card in {}, {}",
                ord,
                cloc,
                prop,
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum Consequent<C: Card>
where
    Standard: Distribution<C>,
{
    Draw(usize, bool),
    Put(usize, CardOrdering<C>, CardLocation, bool),
    Bet(Option<(NumberOrdering, TokenLocation)>),
    Gain(TokenLocation),
    PlayIt(bool),
    Next,
    Done,
    Out,
    Win,
    End,
}

impl<C: Card> Display for Consequent<C>
where
    Standard: Distribution<C>,
    OrdProperty<C>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            &Consequent::Draw(num, hidden) =>
                write!(
                    f,
                    "draw {} card{} from the deck, face {}",
                    num,
                    if num == 1 {
                        ""
                    } else {
                        "s"
                    },
                    if hidden {
                        "down"
                    } else {
                        "up"
                    },
                ),
            Consequent::Put(num, CardOrdering(ord, prop), cloc, hidden) =>
                write!(
                    f,
                    "put {} card{} face {} from the player's hand into {}, only if they are all {} the \
                     top card there {}",
                    num,
                    if *num == 1 {
                        ""
                    } else {
                        "s"
                    },
                    if *hidden {
                        "without revealing"
                    } else {
                        "face up"
                    },
                    cloc,
                    ord,
                    prop,
                ),
            Consequent::Bet(ord_tloc) => if let Some((ord, tloc)) = ord_tloc {
                write!(
                    f,
                    "bet an amount of tokens {} the number of tokens in {}",
                    ord,
                    tloc,
                )
            } else {
                f.write_str("bet some tokens")
            },
            Consequent::Gain(tloc) => write!(f, "gain all the tokens in {}", tloc),
            &Consequent::PlayIt(hidden) => write!(
                f,
                "play all the cards specified in the condition into the place specified by the \
                 condition, face {}",
                if hidden {
                    "down"
                } else {
                    "up"
                },
            ),
            Consequent::Next => write!(f, "this player's turn is over"),
            Consequent::Done => write!(f, "this player is done for this stage"),
            Consequent::Out => write!(f, "this player is out of the game"),
            Consequent::Win => write!(f, "this player wins and the game ends"),
            Consequent::End => write!(f, "the game immediately ends"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum ComputerConsequent {
    Deal(usize, Vec<usize>),
    Place(usize, CardLocation),
    Give(usize, Vec<usize>),
}

impl Display for ComputerConsequent {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ComputerConsequent::Deal(num, players) => {
                write!(f, "deal {} card{} face down to players ", num, if *num == 1 {
                    ""
                } else {
                    "s"
                })?;

                let players = players.iter().map(|&p| p + 1).collect::<Vec<_>>();

                display_list(
                    f,
                    &players,
                    |_| unreachable!("there must always be at least one player to deal to"),
                )
            }
            &ComputerConsequent::Place(num, cloc) => write!(
                f,
                "place {} card{} in {}",
                num,
                if num == 1 {
                    ""
                } else {
                    "s"
                },
                cloc,
            ),
            ComputerConsequent::Give(num, players) => {
                write!(f, "give {} token{} to players ", num, if *num == 1 {
                    ""
                } else {
                    "s"
                })?;

                let players = players.iter().map(|&p| p + 1).collect::<Vec<_>>();

                display_list(
                    f,
                    &players,
                    |_| unreachable!("there must always be at least one player to give to"),
                )
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Rule<C: Card>(pub RuleNecessity, pub Antecedent<C>, pub Consequent<C>)
where
    Standard: Distribution<C>;

impl<C: Card + Display> Display for Rule<C>
where
    Standard: Distribution<C>,
    EqProperty<C>: Display,
    OrdProperty<C>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, " * necessity: {}\n * condition: {}\n * action: {}", &self.0, &self.1, &self.2)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct ComputerRule(pub ComputerConsequent);

impl Display for ComputerRule {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, " * {}", &self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Stage<C: Card>(pub Vec<ComputerRule>, pub Vec<Rule<C>>)
where
    Standard: Distribution<C>;

impl<C: Card + Display> Display for Stage<C>
where
    Standard: Distribution<C>,
    EqProperty<C>: Display,
    OrdProperty<C>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(
            "At the beginning of this stage, the computer will (in order of execution):\n",
        )?;

        for crule in &self.0 {
            Display::fmt(crule, f)?;
            f.write_str("\n")?;
        }

        f.write_str("\nThen, the players may choose an action to perform:\n")?;

        let mut rules = self.1.clone();
        rules.sort_unstable_by(|Rule(l, ..), Rule(r, ..)| l.cmp(r));

        for rule in &rules {
            Display::fmt(rule, f)?;
            f.write_str("\n")?;
        }

        Ok(())
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Game<C: Card>
where
    Standard: Distribution<C>,
{
    pub stages: Vec<Stage<C>>,
    pub ranker: Ranker<C>,
    pub score_per_remaining_token: Score,
    pub score_for_living: Score,
    pub empty_card_loc: HashMap<CardLocation, EmptyCardLocBehavior>,
}

impl<C: Card + Debug> Debug for Game<C>
where
    Standard: Distribution<C>,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("Game")
            .field("stages", &self.stages)
            .field("ranker", &self.ranker)
            .field("score_per_remaining_token", &self.score_per_remaining_token)
            .field("score_for_living", &self.score_for_living)
            .field("empty_card_loc", &self.empty_card_loc)
            .finish()
    }
}

impl<C: Card + Display> Display for Game<C>
where
    Standard: Distribution<C>,
    EqProperty<C>: Display,
    OrdProperty<C>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(" ***  STAGES ***\n\n")?;

        for (num, stage) in self.stages.iter().enumerate() {
            writeln!(f, "  ** Stage {} **\n{}", num + 1, stage)?;
        }

        writeln!(
            f,
            "\n *** CARD RANKING ***\n\n{}\n  ***  When card locations are empty  ***\n",
            &self.ranker,
        )?;

        for (cloc, bhv) in &self.empty_card_loc {
            writeln!(f, "When {} is empty, {}.", cloc, bhv)?;
        }

        writeln!(
            f,
            "\n *** EXTRA ***\n\n\
             Every player not out at the end gains {} points.\n\
             Every player gains {} points for every token they still have.",
            &self.score_for_living,
            &self.score_per_remaining_token,
        )
    }
}

impl<C: Card> Game<C>
where
    Standard: Distribution<C>,
{
    fn update_table_count(
        &mut self,
        rng: &mut impl Rng,
        dist: &GameDists<C, impl GameDistsSpec<C>>,
        tables: usize,
    ) {
        todo!()
    }

    pub fn mutate(&mut self, rng: &mut impl Rng, dist: &GameDists<C, impl GameDistsSpec<C>>) {
        match rng.gen_range(0..6) {
            0 => todo!(),
            1 => todo!(),
            2 => todo!(),
            3 => todo!(),
            4 => {
                let tables = dist.tables.sample(rng);
                self.update_table_count(rng, dist, tables);
            },
            5 => { self.empty_card_loc.insert(dist.sample(rng), dist.sample(rng)); },
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

fn display_list(
    f: &mut Formatter,
    stuff: &[impl Display],
    default: impl FnOnce(&mut Formatter) -> FmtResult,
) -> FmtResult {
    display_list_heads(f, stuff, default, |_| Ok(()), |_| Ok(()), |_| Ok(()))
}

fn display_list_heads(
    f: &mut Formatter,
    stuff: &[impl Display],
    default: impl FnOnce(&mut Formatter) -> FmtResult,
    one: impl FnOnce(&mut Formatter) -> FmtResult,
    two: impl FnOnce(&mut Formatter) -> FmtResult,
    many: impl FnOnce(&mut Formatter) -> FmtResult,
) -> FmtResult {
    match stuff {
        [] => default(f),
        [start] => { one(f)?; write!(f, "{}", start) },
        [start, end] => { two(f)?; write!(f, "{} and {}", start, end) },
        [start, mid @ .., end] => {
            many(f)?;
            write!(f, "{}", start)?;

            for thing in mid {
                write!(f, ", {}", thing)?;
            }

            write!(f, ", and {}", end)
        }
    }
}
