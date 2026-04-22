# Tlon: Obligatory Lottery Markets

Tlon is a game about lotteries and markets on top of lotteries. The basic idea is that there are a wide variety of tokens available for purchase and sale and players compete (both with each other and with NPCs) to "survive" (i.e. remain in at least one approved market\*) until the end of the game. Players (humans) and NPCs are collectively referred to _entities_. The Government owns and controls a specific market with a mildly negative EV lottery required to participate in it. 

\* Approved markets are, by default, Government run. But in theory they could be others, really it's just a specific list of markets.

My long term plan is for the client to be a web app with a backend server running elsewhere.

## Project structure

The project should be split up into two halves: An API and a game. The API should be relatively minimal and enforce less than the game. Rules which should not be enforced by the API are marked as unofficial, but this is non-comprehensive. Exercise taste and/or ask. This is sort of non-strict. If a feature could be used to simulate some weird cryptocurrency thing, it should be API. If it's about a specific market (e.g. the notion of the Government) it should be game.

## Game Structure

An instance of the game has $N$ players, who are registered up front in an instance of the game. Unofficially, players are one of two kinds of entities, the other being NPCs. In the API they are all considered entities.

The game progresses in rounds, which are played simultaneously across all players. The rounds have an optional time limit of $T$ seconds.

Each round, entities may submit any number of _orders_. Orders are detailed in the next section, but broadly are actions on a market. Orders are resolved after all players and NPCs submit orders (including a null order). 

A specific set of players are known as Governments. Governments are "immortal" entities which control a set of default markets. They have an unofficial role as the only markets by default visible at the start of the game.

(unofficially) All players begin in at least one market, run by a Government. New markets are advertised by tokens, sold on other markets, which grant entry into the market.

## Markets, Tokens, Orders, and Derivatives

The game includes multiple markets, on which assets are sold. Assets are simplified versions of financial assets. 

### Markets

_Markets_ are spaces for trading to occur. They are "owned" by a specific entity, which has an exclusive right to set mandatory purchase and sales on that market.

### Assets

_Tokens_ are kinds of _assets_. All of them can be purchased and sold, and take a number of different forms.

_Tokens_ are abstract objects which are specified by 3 letters and 3 numbers specified by a dash, e.g. ABC-123, XYZ-987, etc. All of them are completely discrete. Some of them are limited in quantity, although this limit may change (e.g. by issuance of further tokens). They are probably most similar to cryptocurrency assets, but carry no specific meaning. The precise meaning of any given token is left as an exercise to the player, they are meant to be kind of mysterious.

### Orders

Orders are actions on the market. Really, there are three: Buy, sell, and issue. Buy and sell are resolved in the usual way (price first, then FIFO).

Issue orders create new kinds of tokens. Tokens may be totally abstract, but more commonly they are derivative contracts. These should be a flexible framework in the API but a few motivating examples are:
- Lotteries. Tokens issued by a lottery issuance may be purchased to get a chance at a payout. These tokens must be purchased for nothing at expiry time by the issuer (and conversely sold by the bearer) and are resolved automatically. Draws can admit no winner (like the numbers game) or be drawn only from bearers at the option of the issuer.
- Markets. New markets may be created by anyone. These issue tokens which can be used in the market. Markets may require the purchase or sale of certain other tokens in some quantities to enter. This may be a recurring or one-time cost.
- Futures. These work as expected.
- Options. Also work as expected.

## The Default Game

This market is basically trivial but represents a straightforward way to test the game.

The default game includes a single approved market, run by a single Government, which issues a token called TLN-001 that grants access to the market for a single turn. The token is issued as a lottery ticket. The ticket costs 1 TLN-001 and the lottery pays out 1 TLN-001 to one bearer at random. Effectively one participant gets their deposit back. Players start with $S$ TLN-001.

## Narrow v1 Specification

This section narrows the design above into a concrete first implementation. The goal of v1 is to produce a pure game engine that can run the default game with enough real trading to exercise the market rules, while deferring more general derivatives and market creation until later.

### Scope

v1 includes:

- one immortal Government entity
- $N$ mortal entities registered up front
- one approved market
- one survival token, TLN-001
- three abstract tradable tokens, TLN-101, TLN-102, and TLN-103
- round-based simultaneous play
- round-local limit orders with price-time matching
- Government redemption of abstract tokens into TLN-001 at published per-round rates
- a small per-round Government grant of abstract tokens to surviving mortals
- a recurring survival stake in TLN-001
- a single random refund of that stake each round

v1 does not include:

- futures
- options
- player-created markets
- standing orders across rounds
- market orders
- fractional quantities or prices

### Entities

An _entity_ is any participant in the engine. In v1 there are two kinds:

- _Government_: immortal, controls the approved market, publishes redemption values, and wins by default if all mortal entities are eliminated
- _Player_: mortal entity with a display name; for now these may be generated placeholder names

Only mortal entities may be eliminated.

### Assets

All assets are discrete tokens. Internal identity should not rely only on the display form `ABC-123`; that string is a presentation detail.

v1 defines four assets:

- `TLN-001`: the access and survival token
- `TLN-101`: abstract tradable token
- `TLN-102`: abstract tradable token
- `TLN-103`: abstract tradable token

At game start:

- each mortal entity begins with `S` units of `TLN-001`
- each mortal entity begins with a small random bundle of `TLN-101`, `TLN-102`, and `TLN-103`
- Government begins with a large reserve of all four assets

At the start of each new round after settlement, each surviving mortal entity receives a small Government grant of abstract tokens for the next round. This keeps the abstract-token market alive after redemption rather than collapsing after the first cycle.

### Market Model

There is one approved market in v1, owned by the Government.

The market uses an ephemeral round book:

- orders are submitted during a round
- matching occurs once, after submissions close
- partial fills are allowed
- any unfilled remainder expires at the end of the round

The intended future extension point is to support other matching policies, including a uniform-price batch auction, behind the same settlement interface.

### Order Model

v1 supports limit orders only.

Each order contains:

- submitting entity
- market
- side (`buy` or `sell`)
- base asset
- quote asset
- quantity, in whole units of the base asset
- limit price, in whole units of the quote asset per 1 unit of the base asset
- an order identifier used for deterministic tie-breaking

All trade is token-denominated. There is no floating-point money and no fractional pricing.

#### Validation Rules

An order is valid only if:

- the entity is active in the market at the start of the round
- the entity is not attempting to trade with itself
- quantity is positive
- price is positive
- the asset pair is permitted on that market
- the entity has sufficient reservable inventory

Reservation is mandatory:

- a buy order locks the full worst-case quote amount at submission time
- a sell order locks the full base quantity at submission time

Reserved assets cannot be spent again during the same round. Any unused reserved amount is released after settlement.

### Matching Rules

v1 uses price-time priority:

- better price first
- for equal price, earlier submission first
- for truly simultaneous ties, lower order identifier first

Matching is deterministic given the round inputs and seed.

Self-trading is disallowed. If two orders would match but belong to the same entity, they do not trade with each other.

### Round Visibility

The game is simultaneous by round, so current-round submissions are hidden until resolution.

Visible at round start:

- current holdings
- current redemption values published by Government
- previous round's submitted orders, fills, expirations, and lottery result
- market definitions and approved-market list

Hidden until resolution:

- current round submitted orders

Visible after resolution:

- the full current-round order book
- all fills and expirations
- redemption outcomes
- survival stake results
- the lottery refund result

### Government Redemption

At the start of each round, Government publishes a redemption table giving the number of `TLN-001` tokens paid for each abstract token.

Example:

- `TLN-101 -> 1 TLN-001`
- `TLN-102 -> 2 TLN-001`
- `TLN-103 -> 0 TLN-001`

These values may vary from round to round according to a deterministic seeded process or a predefined schedule.

Redemption is a separate settlement phase in v1, not an ordinary order-book interaction. This keeps player-to-player trading distinct from the Government's role as issuer and redeemer.

### Survival Rule

To remain in the approved market for the next round, each mortal entity must pay a survival stake of 1 `TLN-001` each round.

After all normal fills and redemptions resolve:

- each eligible mortal entity pays 1 `TLN-001`
- one eligible mortal entity is selected at random to receive 1 `TLN-001` back

Effectively, one participant gets that round's stake refunded.

An entity that cannot pay the survival stake is eliminated at the end of the round.

### Victory Rule

The game ends when no mortal entities remain.

If all mortal entities are eliminated in the same round, Government wins by default.

If exactly one mortal entity remains after elimination, that entity is the winner.

### Default Round Sequence

The v1 engine should model one round as the following ordered pipeline:

1. publish round-start view
2. collect orders from active mortal entities and any NPC policies later added
3. validate orders and reserve inventory
4. match orders using the configured matching policy
5. settle fills and release unused reserves
6. apply Government redemption of abstract tokens into `TLN-001`
7. apply the survival stake and random refund
8. if the game continues, grant abstract tokens for the next round and publish the next round's redemption values
9. eliminate mortal entities with no approved-market access for the next round
10. publish a complete round report
11. check for victory

### Implementation Architecture

The first implementation should keep the engine pure and deterministic. The core stepping function should take explicit round inputs and produce a new state plus an event log.

A suitable module split is:

- `Tlon`: small public surface and re-exports
- `Tlon.Core.Types`: identifiers, entities, assets, markets, orders, fills
- `Tlon.Core.State`: game state, holdings, reservations, visibility snapshots
- `Tlon.Core.Engine`: round stepping pipeline
- `Tlon.Core.OrderBook`: price-time matching
- `Tlon.Core.Event`: round reports and audit events
- `Tlon.Core.Rng`: deterministic seeded randomness
- `Tlon.Game.Default.Config`: `N`, `S`, seeds, token bundle parameters
- `Tlon.Game.Default.Config`: `N`, `S`, seeds, token bundle and round-grant parameters
- `Tlon.Game.Default.Setup`: initial game construction
- `Tlon.Game.Default.Rules`: redemption schedule, survival stake, elimination
- `Tlon.Game.Default.View`: simple text rendering for a CLI runner

The matching engine should be abstracted behind a policy boundary so that v1 can use price-time priority while later versions may add uniform-price batch matching without reshaping the rest of the engine.

Conceptually:

- `stepRound :: RoundInputs -> GameState -> (GameState, [GameEvent])`
- `matchRound :: MatchingPolicy -> [ValidatedOrder] -> [Fill]`

### Open Questions Deferred Past v1

The following are intentionally postponed until after the default game is working:

- generalized issuance for derivatives
- market creation by players
- persistent order books
- richer visibility rules across multiple markets
- alternate end conditions beyond last-mortal-standing
