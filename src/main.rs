use rand::prelude::*;
use rand_distr::{Standard, Uniform, Binomial};

mod rules;
mod mcts;
mod genetic;
use rules::{*, card::Common52Card};

struct GameDistSpec;

impl GameDistsSpec<card::Common52Card> for GameDistSpec {
    type PlayersDist = Uniform<usize>;
    type TablesDist = Uniform<usize>;
    type DrawDist = Uniform<usize>;
    type PutDist = Uniform<usize>;
    type BetDist = Uniform<usize>;
    type OrdDist = Standard;
    type EqDist = Standard;
    type DealDist = Uniform<usize>;
    type DealPlaceDist = Uniform<usize>;
    type PlaceDist = Uniform<usize>;
    type GiveDist = Uniform<usize>;
    type ScoreDist = Uniform<Score>;
    type PlayDist = Uniform<usize>;
    type RulesDist = BinomialUsize;
    type ComRulesDist = BinomialUsize;
    //type RulesDist = Uniform<usize>;
    //type ComRulesDist = Uniform<usize>;
    type StageDist = Uniform<usize>;
}

#[derive(Clone)]
pub struct BinomialUsize(Binomial);

impl Distribution<usize> for BinomialUsize {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> usize {
        self.0.sample(rng) as usize
    }
}

fn main() {
    let distr = GameDists::<Common52Card, GameDistSpec> {
        players: Uniform::new(0, 4),
        tables: Uniform::new_inclusive(0, 2),
        draws: Uniform::new_inclusive(1, 3),
        puts: Uniform::new_inclusive(1, 3),
        bets: Uniform::new_inclusive(1, 4),
        ords: Standard,
        eqs: Standard,
        deals: Uniform::new_inclusive(1, 5),
        deal_places: Uniform::new_inclusive(1, 4),
        places: Uniform::new_inclusive(1, 5),
        gives: Uniform::new_inclusive(1, 5),
        score: Uniform::new_inclusive(-100, 100),
        rules: BinomialUsize(Binomial::new(10, 0.5).unwrap()),
        comrules: BinomialUsize(Binomial::new(10, 0.8).unwrap()),
        //rules: Uniform::new(0, 5),
        //comrules: Uniform::new(0, 5),
        stages: Uniform::new_inclusive(1, 10),
        max_card_locs: 4,
        max_have_conditions: 8,
        max_have_cards: 8,
        max_places: 2,
        max_players: 4,
        max_combo_size: 10,
        max_combos: 10,
        exact_players: 4,
        exact_tables: 2,
    };

    let game: Game<_> = thread_rng().sample(&distr);
    //println!("{}", ron::ser::to_string_pretty(&game, ron::ser::PrettyConfig::default()).unwrap());
    println!("{}", game);
}
