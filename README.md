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

## Map Analysis & Segmentation

The framework includes advanced map analysis capabilities in `SC2.Grid.Algo`, enabling strategic positioning and terrain-aware decision making.

### Chokepoint Detection

Automatically identify narrow passages and strategic control points using raycasting algorithms:

```haskell
-- Find a chokepoint from a starting position
findChokePoint :: Grid -> Int -> TilePos -> Maybe [TilePos]

-- Find all significant chokepoints on the map
findAllChokePoints :: Grid -> ([Ray], Grid)
```

**How it works:**
- Casts rays in multiple directions (0°, 45°, 90°, 135°, 180°) from each open tile
- Detects narrow passages where opposing rays meet within a threshold distance
- Validates that chokepoints separate regions of sufficient volume (configurable minimum)
- Returns ray coordinates representing the chokepoint geometry

**Use cases:**
- Position defensive units at chokepoints
- Wall-off narrow passages with buildings
- Identify attack/retreat paths through narrow terrain
- Plan expansion placement away from vulnerable chokepoints

### Region Segmentation

Partition the map into distinct regions connected by chokepoints:

```haskell
-- Segment map into distinct regions
gridSegment :: Grid -> [(RegionId, Region)]

-- Build adjacency graph of regions
buildRegionGraph :: [(RegionId, Region)] -> RegionLookup -> RegionGraph

-- Lookup which region a tile belongs to
buildRegionLookup :: [(RegionId, Region)] -> RegionLookup
```

**How it works:**
- Uses flood-fill to identify contiguous walkable areas
- Each region is a connected component of open tiles
- Builds graph showing which regions are adjacent (connected through chokepoints)
- Provides O(1) lookup from tile position to region ID

**Use cases:**
- Strategic map control: identify which regions you control vs enemy
- Pathfinding: find sequence of regions to traverse from A to B
- Expansion planning: identify isolated regions suitable for bases
- Threat assessment: determine if enemy can reach your base without crossing chokepoints

### Region Graph Pathfinding

Navigate between regions using BFS on the region graph:

```haskell
-- Find shortest path (in regions) from start to end
regionGraphBfs :: RegionGraph -> RegionId -> RegionId -> [RegionId]
```

**Use cases:**
- High-level strategic pathfinding (region-to-region, then detailed within region)
- Identify how many chokepoints separate two locations
- Plan attack routes that minimize chokepoint traversals
- Predict enemy attack vectors

### Example: Strategic Map Analysis

```haskell
import SC2.Grid.Algo

analyzeMap :: Grid -> (RegionGraph, RegionLookup)
analyzeMap grid = (regionGraph, regionLookup)
  where
    -- Segment map into regions
    regions = gridSegment grid
    
    -- Build lookup table
    regionLookup = buildRegionLookup regions
    
    -- Build region adjacency graph
    regionGraph = buildRegionGraph regions regionLookup

-- Find path from your base to enemy base
findAttackPath :: TilePos -> TilePos -> RegionGraph -> RegionLookup -> [RegionId]
findAttackPath myBase enemyBase rg rl = 
  regionGraphBfs rg startRegion endRegion
  where
    Just startRegion = HashMap.lookup myBase rl
    Just endRegion = HashMap.lookup enemyBase rl

-- Identify all chokepoints
findDefensivePositions :: Grid -> [Ray]
findDefensivePositions grid = chokepoints
  where
    (chokepoints, _) = findAllChokePoints grid
```

### Grid Utilities

Additional grid analysis functions:

- **`gridBfs`** — Breadth-first search with custom transitions and goal predicates
- **`gridRaycastTile`** — Cast ray from origin in direction until obstacle
- **`gridSplitByRay`** — Split grid into regions separated by a ray (chokepoint)
- **`checkVolumes`** — Verify regions separated by ray meet minimum size requirements
- **`complementRegionLookup`** — Extend region lookup to tiles near region boundaries

### Implementation Notes

- **Performance:** Unboxed grids (`Data.Vector.Unboxed`) for cache-efficient operations
- **Precision:** All algorithms work on tile-level granularity (grid cells, not game coordinates)
- **Validation:** Chokepoint detection includes volume checks to avoid false positives in open areas
- **Configurability:** Minimum region sizes and chokepoint thresholds are parameterized

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
