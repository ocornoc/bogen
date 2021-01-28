use std::hint::unreachable_unchecked;
use rand::prelude::*;
use rand_distr::Distribution;
use super::*;

pub trait GameDistsSpec<C: Card>
where
    Standard: Distribution<C>,
{
    type PlayersDist: Distribution<usize> + Clone;
    type TablesDist: Distribution<usize> + Clone;
    type DrawDist: Distribution<usize> + Clone;
    type PutDist: Distribution<usize> + Clone;
    type BetDist: Distribution<usize> + Clone;
    type OrdDist: Distribution<OrdProperty<C>> + Clone;
    type EqDist: Distribution<EqProperty<C>> + Clone;
    type DealDist: Distribution<usize> + Clone;
    type DealPlaceDist: Distribution<usize> + Clone;
    type PlaceDist: Distribution<usize> + Clone;
    type GiveDist: Distribution<usize> + Clone;
    type ScoreDist: Distribution<Score> + Clone;
    type PlayDist: Distribution<usize> + Clone;
    type RulesDist: Distribution<usize> + Clone;
    type ComRulesDist: Distribution<usize> + Clone;
    type StageDist: Distribution<usize> + Clone;
}

pub struct GameDists<C: Card, Spec: GameDistsSpec<C>>
where
    Standard: Distribution<C>,
{
    pub players: Spec::PlayersDist,
    pub tables: Spec::TablesDist,
    pub draws: Spec::DrawDist,
    pub puts: Spec::DrawDist,
    pub bets: Spec::PutDist,
    pub ords: Spec::OrdDist,
    pub eqs: Spec::EqDist,
    pub deals: Spec::DealDist,
    pub deal_places: Spec::DealPlaceDist,
    pub places: Spec::PlaceDist,
    pub gives: Spec::GiveDist,
    pub score: Spec::ScoreDist,
    pub rules: Spec::RulesDist,
    pub comrules: Spec::ComRulesDist,
    pub stages: Spec::StageDist,
    pub max_card_locs: usize,
    pub max_have_conditions: usize,
    pub max_have_cards: usize,
    pub max_places: usize,
    pub max_players: usize,
    pub max_combo_size: usize,
    pub max_combos: usize,
    pub exact_players: usize,
    pub exact_tables: usize,
}

impl<C: Card, Spec: GameDistsSpec<C>> Clone for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn clone(&self) -> Self {
        GameDists {
            players: self.players.clone(),
            tables: self.tables.clone(),
            draws: self.draws.clone(),
            puts: self.puts.clone(),
            bets: self.bets.clone(),
            ords: self.ords.clone(),
            eqs: self.eqs.clone(),
            deals: self.deals.clone(),
            deal_places: self.deal_places.clone(),
            places: self.places.clone(),
            gives: self.gives.clone(),
            score: self.score.clone(),
            rules: self.rules.clone(),
            comrules: self.comrules.clone(),
            stages: self.stages.clone(),
            max_card_locs: self.max_card_locs,
            max_have_conditions: self.max_have_conditions,
            max_have_cards: self.max_have_cards,
            max_places: self.max_places,
            max_players: self.max_players,
            max_combo_size: self.max_combo_size,
            max_combos: self.max_combos,
            exact_players: self.exact_players,
            exact_tables: self.exact_tables,
        }
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<OrdProperty<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> OrdProperty<C> {
        self.ords.sample(rng)
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<EqProperty<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> EqProperty<C> {
        self.eqs.sample(rng)
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<CardOrdering<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> CardOrdering<C> {
        CardOrdering(rng.gen::<NumberOrdering>(), self.sample(rng))
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<CardLocation> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> CardLocation {
        match rng.gen_range(0..3) {
            0 => CardLocation::SpecificHand(self.players.sample(rng)),
            1 => CardLocation::MyHand,
            2 => CardLocation::Table(self.tables.sample(rng)),
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<TokenLocation> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TokenLocation {
        match rng.gen_range(0..3) {
            0 => TokenLocation::SpecificBet { player: self.players.sample(rng), stash: rng.gen::<bool>() },
            1 => TokenLocation::MyBet,
            2 => TokenLocation::AllOthersBets,
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<EmptyCardLocBehavior> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> EmptyCardLocBehavior {
        match rng.gen_range(0..6) {
            0 => EmptyCardLocBehavior::Tie,
            1 => EmptyCardLocBehavior::Die,
            2 => EmptyCardLocBehavior::End,
            3 => EmptyCardLocBehavior::Win(self.players.sample(rng)),
            4 => EmptyCardLocBehavior::RespawnAndReshuffle,
            5 => EmptyCardLocBehavior::TakeAndReshuffle {
                places: {
                    let mut places = sample_many_nonempty(
                        rng,
                        self,
                        self.max_places,
                        |rng, dist| dist.sample(rng),
                    );
                    places.sort_unstable();
                    places.dedup();
                    places
                },
                fail: Box::new(match rng.gen_range(0..5) {
                    0 => EmptyCardLocBehavior::Tie,
                    1 => EmptyCardLocBehavior::Die,
                    2 => EmptyCardLocBehavior::End,
                    3 => EmptyCardLocBehavior::Win(self.players.sample(rng)),
                    4 => EmptyCardLocBehavior::RespawnAndReshuffle,
                    //_ => unsafe { unreachable_unchecked() },
                    _ => unreachable!(),
                }),
            },
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<HaveCondition<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> HaveCondition<C> {
        match rng.gen_range(0..2) {
            0 => HaveCondition::PropertyOverlap(self.sample(rng)),
            1 => HaveCondition::PropertyExclusive(self.sample(rng)),
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<Ranker<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Ranker<C> {
        Ranker(sample_many(
            rng,
            self,
            self.max_combos,
            |rng, dist| (
                {
                    let mut combo = sample_many_nonempty(
                        rng,
                        (),
                        dist.max_combo_size,
                        |rng, _|
                        rng.gen()
                    );
                    combo.sort_unstable_by(|l, other| l.cmp_prop(other, C::DEFAULT_ORD));
                    combo
                },
                dist.score.sample(rng),
            ),
        ))
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<Antecedent<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Antecedent<C> {
        match rng.gen_range(0..7) {
            0 => Antecedent::Unconditional,
            1 => Antecedent::Tokens(self.sample(rng), rng.gen::<NumberOrdering>(), self.sample(rng)),
            2 => Antecedent::Play(
                sample_many_nonempty(rng, self, self.max_card_locs, |rng, dist| dist.sample(rng)),
                self.sample(rng),
                sample_many_nonempty(rng, self, self.max_card_locs, |rng, dist| dist.sample(rng)),
            ),
            3 => Antecedent::Sum(
                sample_many_nonempty(rng, self, self.max_card_locs, |rng, dist| dist.sample(rng)),
                rng.gen::<NumberOrdering>(),
                sample_many_nonempty(rng, self, self.max_card_locs, |rng, dist| dist.sample(rng)),
            ),
            4 => Antecedent::Have(sample_many_nonempty(
                rng,
                self,
                self.max_have_conditions,
                |rng, dist| (
                    dist.sample(rng),
                    sample_many_nonempty(rng, (), dist.max_have_cards, |rng, _| rng.gen()),
                ),
            )),
            5 => Antecedent::Draw(self.draws.sample(rng), self.sample(rng), rng.gen::<bool>()),
            6 => Antecedent::Show(self.sample(rng), self.sample(rng)),
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<Consequent<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Consequent<C> {
        match rng.gen_range(0..10) {
            0 => Consequent::Draw(self.draws.sample(rng), rng.gen::<bool>()),
            1 => Consequent::Put(
                self.puts.sample(rng),
                self.sample(rng),
                self.sample(rng),
                rng.gen::<bool>(),
            ),
            2 => Consequent::Bet(if rng.gen::<bool>() {
                Some((rng.gen::<NumberOrdering>(), self.sample(rng)))
            } else {
                None
            }),
            3 => Consequent::Gain(self.sample(rng)),
            4 => Consequent::PlayIt(rng.gen::<bool>()),
            5 => Consequent::Next,
            6 => Consequent::Done,
            7 => Consequent::Out,
            8 => Consequent::Win,
            9 => Consequent::End,
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<ComputerConsequent> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> ComputerConsequent {
        match rng.gen_range(0..3) {
            0 => ComputerConsequent::Deal(
                self.deals.sample(rng),
                {
                    let deals = self.deals.sample(rng);
                    (&self.players)
                        .sample_iter(rng)
                        .take(deals)
                        .collect()
                },
            ),
            1 => ComputerConsequent::Place(self.places.sample(rng), self.sample(rng)),
            2 => ComputerConsequent::Give(self.gives.sample(rng), sample_many_nonempty(
                rng,
                self,
                self.max_players,
                |rng, dist| dist.players.sample(rng),
            )),
            //_ => unsafe { unreachable_unchecked() },
            _ => unreachable!(),
        }
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<Rule<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Rule<C> {
        let mut ant = self.sample(rng);
        let con = self.sample(rng);

        if matches!(con, Consequent::PlayIt(_)) {
            ant = Antecedent::Show(self.sample(rng), self.sample(rng));
        }

        Rule(rng.gen::<RuleNecessity>(), ant, con)
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<ComputerRule> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> ComputerRule {
        ComputerRule(self.sample(rng))
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<Stage<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Stage<C> {
        let comrules = self.comrules.sample(rng);
        let rules = self.rules.sample(rng);

        Stage(
            self.sample_iter(&mut *rng).take(comrules).collect(),
            self.sample_iter(rng).take(rules).collect(),
        )
    }
}

impl<C: Card, Spec: GameDistsSpec<C>> Distribution<Game<C>> for GameDists<C, Spec>
where
    Standard: Distribution<C>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Game<C> {
        let stages = self.stages.sample(rng);
        let empty_card_loc = CardLocation::get_all(self.exact_players, self.exact_tables)
            .into_iter()
            .zip(self.sample_iter(&mut *rng))
            .collect();

        Game {
            stages: self.sample_iter(&mut *rng).take(stages).collect(),
            ranker: self.sample(rng),
            score_per_remaining_token: self.score.sample(rng),
            score_for_living: self.score.sample(rng),
            empty_card_loc,
        }
    }
}

fn sample_many<T, U: Copy, R: Rng + ?Sized>(
    rng: &mut R,
    dist: U,
    mut max: usize,
    gen: fn(&mut R, U) -> T,
) -> Vec<T> {
    let mut things = Vec::with_capacity(max);

    while max > 0 && rng.gen() {
        things.push(gen(rng, dist));
        max -= 1;
    }

    things
}

fn sample_many_nonempty<T, U: Copy, R: Rng + ?Sized>(
    rng: &mut R,
    dist: U,
    max: usize,
    gen: fn(&mut R, U) -> T,
) -> Vec<T> {
    let mut things = sample_many(rng, dist, max.checked_sub(1).unwrap(), gen);
    things.push(gen(rng, dist));
    things
}
