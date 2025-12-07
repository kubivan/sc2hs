[![Stand With Ukraine](https://raw.githubusercontent.com/vshymanskyy/StandWithUkraine/main/banner2-direct.svg)](https://stand-with-ukraine.pp.ua)

# SC2HS: StarCraft II Bot Framework in Haskell

A modern, modular Haskell framework for developing StarCraft II bots with a clean separation between low-level API and high-level strategic systems.

## Architecture

The project is organized into three distinct packages:

### 1. **sc2api** — Minimal Bot API
The foundation for all bots. Provides everything needed to write a basic bot without opinions about strategy or tactics.

**Key modules:**
- `Agent` — IO-based agent typeclass (`agentStep :: a -> ResponseObservation -> UnitAbilities -> IO (a, StepPlan)`)
- `Actions` — Unit commands and actions
- `SC2.Launcher.*` — Game orchestration (BotConfig, LoadConfig, Participant, Game)
- `SC2.Proto.*` — Protocol buffer definitions
- `SC2.Client` — Connection management
- `SC2.Ids.*` — Ability, Unit Type, Upgrade IDs
- `Geometry` — Point math and spatial utilities
- `UnitAbilities` — Ability availability and querying

**Example:**
```haskell
data MyBot = MyBot { /* your state */ }

instance Agent MyBot where
  agentStep bot obs abilities = do
    let actions = [ ... ] -- plan actions
    return (bot', StepPlan actions [])
```

### 2. **sc2monad** — StepMonad Framework
An opinionated, high-level framework built on `StepMonad` for strategic and tactical systems. Optional—use only what you need.

**Key modules:**
- `StepMonad` — Core monad stack (Reader/State/Writer) for stateful computations
- `StepMonadUtils` — Utilities and helpers
- `Observation` — Unit/resource queries, clustering, updates
- `Units` — Unit analysis and queries
- `SC2.Grid.*` — Unboxed grid, pathfinding, chokepoint detection
- `SC2.Geometry` — Spatial utilities
- `SC2.Army.*` — High-level unit orchestration
- `SC2.Squad.*` — Squad FSM and micro management
- `SC2.TechTree` — Tech tree (TH-generated from data.json)
- `SC2.Utils` — Common utilities

**Example:**
```haskell
import StepMonad

buildPylons :: StepMonad ()
buildPylons = do
  probes <- unitsOfType ProtossProbe
  nexus <- unitsOfType ProtossNexus
  -- ... strategic logic
```

### 3. **lambdarookie01** — Example Bot
A complete Protoss bot example using both `sc2api` and `sc2monad`.

**Modules:**
- `Main` — Game loop orchestration
- `TestBot` — Agent implementation with multi-phase logic
- `BotDynamicState` — Mutable bot state (grid, army, observation)
- `ArmyLogic` — Macro logic (squad forming, unit production)
- `AgentBulidUtils` — Build order utilities and placement finding

## Getting Started

### Prerequisites
- GHC 9.10.1+ (via Stack)
- StarCraft II client running locally or remote
- bot-config.yaml with map and connection settings

### Build

```bash
stack build
```

Build a specific package:
```bash
stack build sc2api          # Just the API
stack build sc2monad        # Framework + tests
stack build lambdarookie01  # Example bot
```

### Run Tests

```bash
stack test
```
### Run the Bot

```bash
stack run lambdarookie01
```

The bot reads configuration from `bot-config.yaml` and connects to StarCraft II.

## Writing Your Own Bot

### Option 1: Minimal Bot (sc2api only)

Use `sc2api` alone if you want complete control:

```haskell
import Agent
import SC2.Launcher.Game
import SC2.Launcher.Participant

data YourBot = YourBot { /* state */ }

instance Agent YourBot where
  agentStep bot obs abilities = do
    let actions = yourDecisionLogic obs
    return (bot, StepPlan actions [])

main :: IO ()
main = playMatch (buildHostRuntime (Player yourBotInstance) runtimeConfig)
```

### Option 2: StepMonad Bot (full framework)

Use `sc2monad` for higher-level abstractions:

```haskell
import StepMonad
import Agent

data YourBot = YourBot { state :: DynamicState }

instance Agent YourBot where
  agentStep bot obs abilities = do
    let (newState, plan) = runStepMonad strats obs abilities (bot.state)
    return (bot { state = newState }, plan)

strats :: StepMonad ()
strats = do
  defenders <- defendBase
  attackers <- formAttackSquad
  -- ... more logic
```

## Configuration

Create `bot-config.yaml`:

```yaml
# Bot name
botName: YourBotName

# Map info
map:
  filename: "StarCraftIIPath/Maps/YourMap.SC2Map"

# Connection
port: 5000
serverAddress: localhost
```

Run with:
```bash
stack run lambdarookie01 -- --config bot-config.yaml
```

## Key Concepts

### Agent Typeclass
Every bot implements the `Agent` typeclass with `agentStep`:
- Input: current state, observation, available abilities
- Output: updated state + StepPlan (actions, debug commands, chat)

### StepMonad
A Reader/State/Writer stack for stateful computations:
```haskell
StepMonad r s w a = Reader AgentStatic -> State DynamicState -> Writer (StepPlan) -> a
```
- **Read:** Static game info (unit types, abilities, tech tree)
- **State:** Dynamic bot state (units, grid, army)
- **Write:** Accumulate actions and debug info into StepPlan

### StepPlan
The bot's decision output:
```haskell
data StepPlan = StepPlan 
  { actions :: [Action]           -- Unit commands
  , debug :: [DebugCommand]        -- Debug visuals
  }
```

## Testing

Tests cover:
- Grid algorithms (pathfinding, chokepoints, placement)
- Unit queries and clustering
- TechTree queries
- StepMonad operations
- Observation updates

Run with:
```bash
stack test
```

## License

MIT

## Resources

- [StarCraft II API Documentation](https://github.com/Blizzard/s2client-proto)
- [SC2HS GitHub](https://github.com/kubivan/sc2hs)
- [Aiurgaze](https://github.com/kubivan/aiurgaze)
- Haskell Resources: [Haskell.org](https://www.haskell.org)
