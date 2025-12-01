World State & Decision Architecture Plan

Overview
Design a layered world/decision state enabling strategic intent, squad role assignment, and tactical objectives before low-level micro. Emphasis: modular evaluators, hysteresis to avoid thrash, incremental recomputation.

Clarifications Applied
- Region-level only modeling for presence, control, and scoring (no per-tile phantom layer yet).
- Enemy clusters (unseen forces) are forgotten after X game cycles via decay in PhantomPresence.
- Strategy recognition classes: Macro, Cheese, TimingRush, BuildOrder. Signatures loaded from config at startup for pattern matching.
- Removed separate Semi-Static layer; prior contents folded into StrategicState incremental fields.
- Removed dedicated time field (ssTime); use game loop/frame where needed, store lastSeen only per phantom/region control entry.

World State Layers
1. StaticInfo (existing): map geometry, regions, tech/unit data, starting locations.
2. StrategicState (unified dynamic): intent, threat, region control, enemy composition, economy snapshot, phantom presence, strategy inference.
3. Per-Squad TacticalState (augment existing squads): role, target region(s), objective, confidence

Data Structures (New / Revised)
- StrategicState
   { ssIntent :: ArmyIntent
   , ssThreat :: ThreatMap
   , ssRegionControl :: HashMap RegionId RegionControl
   , ssEnemyComp :: EnemyCompEstimate
   , ssEconomy :: EconomySnapshot
   , ssPhantom :: HashMap RegionId PhantomPresence
   , ssStrategy :: Maybe EnemyStrategy }  -- inferred current dominant enemy strategy
- RegionControl { rcOwner :: Maybe Alliance, rcFriendlyPower :: Float, rcEnemyPower :: Float, rcLastVisit :: GameLoop }
- EnemyCompEstimate { eceArchetypes :: HashMap Archetype Int, eceTechTier :: TechTier, eceUpgrades :: Set UpgradeId, eceUncertainty :: Float }
- EconomySnapshot { ecoWorkers :: Int, ecoBasesSecured :: Int, ecoBasesBuilding :: Int, ecoIncomeMinerals :: Float, ecoIncomeGas :: Float, ecoSupplyUsed :: Int, ecoSupplyCap :: Int }
- PhantomPresence { ppProb :: Float, ppLastSeen :: GameLoop, ppDecayRate :: Float }  -- probability unseen enemy force centered here
- EnemyStrategy = Macro | Cheese | TimingRush | BuildOrder StrategyId
- StrategySignature { ssName :: Text, ssEarlyUnits :: [UnitTypeId], ssTimingWindow :: (GameLoop, GameLoop), ssStructures :: [UnitTypeId], ssScoreWeights :: StrategyWeights }
- TacticalObjective = TO_Move RegionEdge | TO_Attack RegionId | TO_Defend RegionId | TO_Scout RegionId | TO_Regroup TilePos | TO_Harass RegionId
- SquadRole = SR_Attack | SR_Defend | SR_Harass | SR_Scout | SR_Regroup

Strategic Decision Tree
Utility nodes produce scores normalized [0,1]. Combined with weights.
Nodes:
- ForceAdvantage = friendlyPower / (enemyPower+ε) in candidate focus region
- MapControl = controlledRegions / totalRegions adjusted by contested penalty
- TechTiming = sigmoid(ourTier - enemyTier) + pendingUpgradeSpike
- HarassOpportunity = (exposedWorkers * exposureFactor) / localDefensePower
- DefensiveThreat = sum threat near own bases (threat falloff weighted by base importance)
Composite Mode Utilities:
Attack: w1*ForceAdvantage + w2*MapControl + w3*TechTimingPositive - w4*DefensiveThreatBuffer
Defend: w5*DefensiveThreat + w6*(1-ForceAdvantage)
Harass: w7*HarassOpportunity + w8*TechTimingPositive - w9*DefensiveThreat
Regroup: w10*(1-ForceAdvantage) + w11*UpcomingPowerSpike + w12*SupplyInefficiency
Hysteresis: retain previous mode unless newUtility - prevUtility > Δ or critical event (base under attack).

Region Scoring for Focus Selection
Score(region) = α*EnemyThreat(region) + β*ExpansionValue(region) - γ*Distance(strongestSquad, region) + δ*IntelUncertainty(region) + ε*MapControlGap(region)
Select max score; secondary regions for harass/scout chosen by high HarassOpportunity and IntelUncertainty.

Role Assignment Algorithm
Inputs: ArmyIntent, region scores, existing squads (size, speed, range profile).
Steps:
1. Compute capability vector per squad: {power, mobility, range, durability}
2. Sort squads by power; assign top to Attack/Defend depending on intent.
3. Assign high mobility small squads to Harass or Scout targets (distinct regions).
4. If Regroup mode: all non-critical defense squads switch to SR_Regroup at rally region (median of friendly positions or safe choke).
5. Bipartite matching (squads ↔ roles) with cost = negative suitability score; Hungarian or greedy is acceptable (prefer greedy for speed now).

Tactical Decision Tree (Per Squad)
Root: Evaluate local power ratio & role intent.
Branches:
- If SR_Attack and LocalPowerRatio >= engageThreshold ⇒ TO_Attack focusRegion
- If SR_Attack and LocalPowerRatio < retreatThreshold ⇒ TO_Regroup fallbackPos
- If SR_Defend and DefensiveThreat spike ⇒ TO_Defend baseRegion
- If SR_Harass ⇒ TO_Harass harassTarget (economic structures / workers)
- If SR_Scout ⇒ TO_Scout scoutRegion (path via unexplored edges)
- If SR_Regroup ⇒ TO_Regroup rallyPoint
Leaves produce TacticalObjective; micro layer (future) consumes objective.

Computation Cadence
- StrategicState update every 8 frames (tunable). ThreatMap recompute throttled: skip if enemy unit set unchanged (hash tags). RegionControl partial update: only regions seen in last visionWindow (e.g. 64 frames); unseen regions decay toward neutral.
- EnemyCompEstimate incremental: update counts from newly seen units; apply decay to old unseen archetypes.
- PhantomPresence decay each cadence: ppProb_new = max 0 (ppProb_prev * exp(-decayRate * ΔtFrames)); reset to 1.0 when enemy units observed in region; forget (remove) if ppProb < ε_threshold.
- Strategy inference reevaluated on key observation events (new tech building, expansion, cluster detection) or every N frames fallback.

Caching & Performance
- ThreatMap: store last result + incremental diff option later.
- RegionControl: sparse recompute; maintain running friendly/enemy power sums.
- PhantomPresence: lightweight (probability + lastSeen) hash; decay is O(k) regions with entries.
- Strategy signatures preloaded; inference uses filtered matching (structures seen, early unit composition) to compute scores once per trigger.
- Utilities: store last values; use EMA smoothing: U_smooth = λ*U_new + (1-λ)*U_prev to reduce jitter.

Integration Points
1. Extend BotDynamicState with strategicState field OR parallel map keyed externally.
2. Strategy signatures loaded at startup (config file) into EnemyModel state.
3. `agentUpdateDsArmy` pipeline: after observation update → strategic update (threat, region control, phantom, enemy comp, strategy) → squad role assignment → squad processing.
4. Replace `squadTransitionFrom` fixed Idle selection with role-driven transitions deriving next FSM state.
5. Provide debug hooks: debugText for ArmyIntent, region score list, role per squad, phantom presence probabilities, inferred strategy.

Module Plan
- SC2.Army.StrategicState: type + updateStrategicState :: StepMonad d StrategicState
- SC2.Army.RegionControl: computeRegionControl :: Observation -> ThreatMap -> HashMap RegionId RegionControl
- SC2.Army.EnemyModel: updateEnemyComp :: Observation -> EnemyCompEstimate -> EnemyCompEstimate
- SC2.Army.PhantomPresence: updatePhantom :: Observation -> StrategicState -> HashMap RegionId PhantomPresence
- SC2.Army.StrategyRecognition: loadSignatures :: Config -> [StrategySignature]; inferStrategy :: Observation -> StrategicState -> Maybe EnemyStrategy
- SC2.Army.DecisionTree: computeArmyIntent :: StrategicInputs -> ArmyIntent
- SC2.Army.RegionScoring: scoreRegions :: StrategicState -> HashMap RegionId Float
- SC2.Squad.RoleAssignment: assignRoles :: StrategicState -> [Squad] -> [(Squad, SquadRole, Maybe RegionId)]
- SC2.Squad.TacticalPlanner: planSquadObjective :: StrategicState -> Squad -> TacticalObjective

Open Weights (Initial Guesses)
ForceAdvantage w1=0.35; MapControl w2=0.15; TechTiming w3=0.15; DefensiveThreat w5=0.30; HarassOpportunity w7=0.25; Regroup weights tuned after testing.
Cheese defense bias weight w_defCheese=0.20 added to DefensiveThreat when inferred Cheese.
Macro harass incentive weight w_harassMacro=0.15 added to HarassOpportunity when inferred Macro.
TimingRush regroup buffer weight w_regroupRush=0.12 added to Regroup when enemy spike approaching.
Hysteresis Δ=0.08; engageThreshold=1.15; retreatThreshold=0.85.

Risk Mitigation
- Overreaction: EMA smoothing + hysteresis.
- Incomplete info: uncertainty + phantom presence drives Scout role selection rather than assumptions.
- Misclassification: strategy scores require threshold > strategyMinScore; else None.
- Performance: throttle heavy recomputations; adopt incremental threat later.

Incremental Implementation Order
1. StrategicState scaffolding + integration into dynamic state.
2. RegionControl basic (friendly/enemy counts via threat map tags).
3. PhantomPresence (decay + update on observation).
4. StrategyRecognition (load signatures + inference hook).
5. DecisionTree (replace simple counts in Planner; incorporate strategy bias weights).
6. Region scoring & focus selection refinement.
7. RoleAssignment mapping squads to roles (strategy-aware adjustments).
8. TacticalPlanner producing objectives; integrate with FSM transitions.
9. Debug instrumentation.
10. Refinement: smoothing, hysteresis, uncertainty & phantom tuning.

Testing Strategy
- Unit tests: region scoring, army intent selection given synthetic inputs.
- Property tests: hysteresis prevents oscillation under minor utility noise.
- Simulation logs: capture intent/role changes across frames; assert minimal thrash.

Future Extensions
- Predictive modeling: expected reinforcement arrival times.
- Economic planner: schedule expansions based on MapControl & DefensiveThreat.
- Influence map enhancements: friendly zone-of-control, path risk.

Next Actions (after approval)
1. Add TODO items for each module and start with StrategicState scaffolding.
