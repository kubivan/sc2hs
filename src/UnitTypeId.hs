module UnitTypeId where

data UnitTypeId =
      Invalid  -- 0,
    | TerranArmory  -- CANCEL, HALT, CANCEL_LAST, RESEARCH_TERRANSHIPWEAPONS, RESEARCH_TERRANVEHICLEANDSHIPPLATING, RESEARCH_TERRANVEHICLEWEAPONS
    | TerranAutoturret  -- SMART, STOP, ATTACK
    | TerranBanshee  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK, BEHAVIOR_CLOAKON, BEHAVIOR_CLOAKOFF
    | TerranBarracks  -- SMART, TRAIN_MARINE, TRAIN_REAPER, TRAIN_GHOST, TRAIN_MARAUDER, CANCEL, HALT, CANCEL_LAST, RALLY_UNITS, LIFT, BUILD_TECHLAB, BUILD_REACTOR
    | TerranBarracksflying  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, LAND, BUILD_TECHLAB, BUILD_REACTOR
    | TerranBarracksreactor  -- CANCEL
    | TerranBarrackstechlab  -- RESEARCH_STIMPACK, RESEARCH_COMBATSHIELD, RESEARCH_CONCUSSIVESHELLS, CANCEL, CANCEL_LAST
    | TerranBattlecruiser  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_YAMATOGUN, EFFECT_TACTICALJUMP, STOP, ATTACK
    | TerranBunker  -- SMART, EFFECT_SALVAGE, CANCEL, HALT, UNLOADALL, STOP, LOAD, RALLY_UNITS, ATTACK, EFFECT_STIM
    | TerranCommandcenter  -- SMART, TRAIN_SCV, MORPH_PLANETARYFORTRESS, MORPH_ORBITALCOMMAND, CANCEL, HALT, LOADALL, UNLOADALL, CANCEL_LAST, LIFT, RALLY_WORKERS
    | TerranCommandcenterflying  -- SMART, MOVE, PATROL, HOLDPOSITION, LOADALL, UNLOADALL, STOP, LAND
    | TerranCyclone  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_LOCKON, CANCEL, STOP, ATTACK
    | TerranEngineeringbay  -- RESEARCH_HISECAUTOTRACKING, RESEARCH_TERRANSTRUCTUREARMORUPGRADE, RESEARCH_NEOSTEELFRAME, CANCEL, HALT, CANCEL_LAST, RESEARCH_TERRANINFANTRYARMOR, RESEARCH_TERRANINFANTRYWEAPONS
    | TerranFactory  -- SMART, TRAIN_SIEGETANK, TRAIN_THOR, TRAIN_HELLION, TRAIN_HELLBAT, TRAIN_CYCLONE, TRAIN_WIDOWMINE, CANCEL, HALT, CANCEL_LAST, RALLY_UNITS, LIFT, BUILD_TECHLAB, BUILD_REACTOR
    | TerranFactoryflying  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, LAND, BUILD_TECHLAB, BUILD_REACTOR
    | TerranFactoryreactor  -- CANCEL
    | TerranFactorytechlab  -- RESEARCH_INFERNALPREIGNITER, RESEARCH_DRILLINGCLAWS, RESEARCH_RAPIDFIRELAUNCHERS, RESEARCH_SMARTSERVOS, CANCEL, CANCEL_LAST
    | TerranFusioncore  -- RESEARCH_BATTLECRUISERWEAPONREFIT, CANCEL, HALT, CANCEL_LAST
    | TerranGhost  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_NUKECALLDOWN, EFFECT_EMP, EFFECT_GHOSTSNIPE, CANCEL, STOP, ATTACK, BEHAVIOR_CLOAKON, BEHAVIOR_CLOAKOFF, BEHAVIOR_HOLDFIREON, BEHAVIOR_HOLDFIREOFF
    | TerranGhostacademy  -- BUILD_NUKE, RESEARCH_PERSONALCLOAKING, CANCEL, HALT, CANCEL_LAST
    | TerranHellion  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_HELLBAT, STOP, ATTACK
    | TerranHelliontank  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_HELLION, STOP, ATTACK
    | TerranLiberator  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_LIBERATORAGMODE, STOP, ATTACK
    | TerranLiberatorag  -- SMART, MORPH_LIBERATORAAMODE, STOP, ATTACK
    | TerranMarauder  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK, EFFECT_STIM
    | TerranMarine  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK, EFFECT_STIM
    | TerranMedivac  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_HEAL, EFFECT_MEDIVACIGNITEAFTERBURNERS, STOP, LOAD, UNLOADALLAT, ATTACK
    | TerranMissileturret  -- SMART, CANCEL, HALT, STOP, ATTACK
    | TerranMule  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, HARVEST_GATHER, HARVEST_RETURN, ATTACK, EFFECT_REPAIR
    | TerranOrbitalcommand  -- SMART, EFFECT_CALLDOWNMULE, EFFECT_SUPPLYDROP, EFFECT_SCAN, TRAIN_SCV, CANCEL_LAST, LIFT, RALLY_WORKERS
    | TerranOrbitalcommandflying  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, LAND
    | TerranPlanetaryfortress  -- SMART, TRAIN_SCV, LOADALL, STOP, CANCEL_LAST, ATTACK, RALLY_WORKERS
    | TerranRaven  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_POINTDEFENSEDRONE, EFFECT_HUNTERSEEKERMISSILE, EFFECT_AUTOTURRET, STOP, ATTACK
    | TerranReaper  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_KD8CHARGE, STOP, ATTACK
    | TerranRefinery  -- CANCEL, HALT
    | TerranRefineryrich  -- 1949,
    | TerranScv  -- SMART, MOVE, PATROL, HOLDPOSITION, BUILD_COMMANDCENTER, BUILD_SUPPLYDEPOT, BUILD_REFINERY, BUILD_BARRACKS, BUILD_ENGINEERINGBAY, BUILD_MISSILETURRET, BUILD_BUNKER, BUILD_SENSORTOWER, BUILD_GHOSTACADEMY, BUILD_FACTORY, BUILD_STARPORT, BUILD_ARMORY, BUILD_FUSIONCORE, HALT, STOP, HARVEST_GATHER, HARVEST_RETURN, ATTACK, EFFECT_SPRAY, EFFECT_REPAIR
    | TerranSensortower  -- CANCEL, HALT
    | TerranSiegetank  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_SIEGEMODE, STOP, ATTACK
    | TerranSiegetanksieged  -- SMART, MORPH_UNSIEGE, STOP, ATTACK
    | TerranStarport  -- SMART, TRAIN_MEDIVAC, TRAIN_BANSHEE, TRAIN_RAVEN, TRAIN_BATTLECRUISER, TRAIN_VIKINGFIGHTER, TRAIN_LIBERATOR, CANCEL, HALT, CANCEL_LAST, RALLY_UNITS, LIFT, BUILD_TECHLAB, BUILD_REACTOR
    | TerranStarportflying  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, LAND, BUILD_TECHLAB, BUILD_REACTOR
    | TerranStarportreactor  -- CANCEL
    | TerranStarporttechlab  -- RESEARCH_BANSHEECLOAKINGFIELD, RESEARCH_RAVENCORVIDREACTOR, RESEARCH_ENHANCEDMUNITIONS, RESEARCH_BANSHEEHYPERFLIGHTROTORS, RESEARCH_RAVENRECALIBRATEDEXPLOSIVES, RESEARCH_HIGHCAPACITYFUELTANKS, RESEARCH_ADVANCEDBALLISTICS, CANCEL, CANCEL_LAST
    | TerranSupplydepot  -- MORPH_SUPPLYDEPOT_LOWER, CANCEL, HALT
    | TerranSupplydepotlowered  -- MORPH_SUPPLYDEPOT_RAISE
    | TerranThor  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_THORHIGHIMPACTMODE, STOP, ATTACK
    | TerranThorap  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_THOREXPLOSIVEMODE, CANCEL, STOP, ATTACK
    | TerranVikingassault  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_VIKINGFIGHTERMODE, STOP, ATTACK
    | TerranVikingfighter  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_VIKINGASSAULTMODE, STOP, ATTACK
    | TerranWidowmine  -- SMART, MOVE, PATROL, HOLDPOSITION, BURROWDOWN, STOP, ATTACK
    | TerranWidowmineburrowed  -- SMART, EFFECT_WIDOWMINEATTACK, BURROWUP
    | TerranKd8charge  -- 830,
    | TerranNuke  -- 58,
    | TerranPointdefensedrone  -- 11,
    | TerranReactor  -- 6,
    | TerranTechlab  -- 5,
    | ZergBaneling  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_EXPLODE, BEHAVIOR_BUILDINGATTACKON, BEHAVIOR_BUILDINGATTACKOFF, BURROWDOWN, STOP, ATTACK
    | ZergBanelingburrowed  -- EFFECT_EXPLODE, BURROWUP
    | ZergBanelingcocoon  -- SMART, CANCEL_LAST, RALLY_UNITS
    | ZergBanelingnest  -- RESEARCH_CENTRIFUGALHOOKS, CANCEL, CANCEL_LAST
    | ZergBroodling  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergBroodlord  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergBroodlordcocoon  -- SMART, MOVE, PATROL, HOLDPOSITION, CANCEL
    | ZergChangeling  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergChangelingmarine  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergChangelingmarineshield  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergChangelingzealot  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergChangelingzergling  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergChangelingzerglingwings  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergCorruptor  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_BROODLORD, EFFECT_CAUSTICSPRAY, STOP, ATTACK
    | ZergCreeptumor  -- CANCEL
    | ZergCreeptumorburrowed  -- SMART, CANCEL, BUILD_CREEPTUMOR
    | ZergCreeptumorqueen  -- CANCEL
    | ZergDrone  -- SMART, MOVE, PATROL, HOLDPOSITION, BUILD_HATCHERY, BUILD_EXTRACTOR, BUILD_SPAWNINGPOOL, BUILD_EVOLUTIONCHAMBER, BUILD_HYDRALISKDEN, BUILD_SPIRE, BUILD_ULTRALISKCAVERN, BUILD_INFESTATIONPIT, BUILD_NYDUSNETWORK, BUILD_BANELINGNEST, BUILD_ROACHWARREN, BUILD_SPINECRAWLER, BUILD_SPORECRAWLER, BURROWDOWN, STOP, HARVEST_GATHER, HARVEST_RETURN, ATTACK, EFFECT_SPRAY
    | ZergDroneburrowed  -- BURROWUP
    | ZergEgg  -- SMART, CANCEL_LAST, RALLY_UNITS
    | ZergEvolutionchamber  -- CANCEL, CANCEL_LAST, RESEARCH_ZERGGROUNDARMOR, RESEARCH_ZERGMELEEWEAPONS, RESEARCH_ZERGMISSILEWEAPONS
    | ZergExtractor  -- CANCEL
    | ZergExtractorrich  -- 1945,
    | ZergGreaterspire  -- CANCEL_LAST, RESEARCH_ZERGFLYERARMOR, RESEARCH_ZERGFLYERATTACK
    | ZergHatchery  -- SMART, MORPH_LAIR, RESEARCH_PNEUMATIZEDCARAPACE, RESEARCH_BURROW, TRAIN_QUEEN, CANCEL, CANCEL_LAST, RALLY_UNITS, RALLY_WORKERS
    | ZergHive  -- SMART, RESEARCH_PNEUMATIZEDCARAPACE, RESEARCH_BURROW, TRAIN_QUEEN, CANCEL_LAST, RALLY_UNITS, RALLY_WORKERS
    | ZergHydralisk  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_LURKER, BURROWDOWN, STOP, ATTACK
    | ZergHydraliskburrowed  -- BURROWUP
    | ZergHydraliskden  -- RESEARCH_GROOVEDSPINES, RESEARCH_MUSCULARAUGMENTS, MORPH_LURKERDEN, CANCEL, CANCEL_LAST
    | ZergInfestationpit  -- RESEARCH_PATHOGENGLANDS, RESEARCH_NEURALPARASITE, CANCEL, CANCEL_LAST
    | ZergInfestedterransegg  -- SMART, MOVE, PATROL, HOLDPOSITION
    | ZergInfestor  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_FUNGALGROWTH, EFFECT_INFESTEDTERRANS, EFFECT_NEURALPARASITE, CANCEL, BURROWDOWN, STOP, ATTACK
    | ZergInfestorburrowed  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_FUNGALGROWTH, EFFECT_INFESTEDTERRANS, EFFECT_NEURALPARASITE, CANCEL, BURROWUP, STOP, ATTACK
    | ZergInfestorterran  -- SMART, MOVE, PATROL, HOLDPOSITION, BURROWDOWN, STOP, ATTACK
    | ZergLair  -- SMART, MORPH_HIVE, RESEARCH_PNEUMATIZEDCARAPACE, RESEARCH_BURROW, TRAIN_QUEEN, CANCEL, CANCEL_LAST, RALLY_UNITS, RALLY_WORKERS
    | ZergLarva  -- TRAIN_DRONE, TRAIN_ZERGLING, TRAIN_OVERLORD, TRAIN_HYDRALISK, TRAIN_MUTALISK, TRAIN_ULTRALISK, TRAIN_ROACH, TRAIN_INFESTOR, TRAIN_CORRUPTOR, TRAIN_VIPER, TRAIN_SWARMHOST
    | ZergLocustmp  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergLocustmpflying  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_LOCUSTSWOOP, STOP, ATTACK
    | ZergLurkerdenmp  -- RESEARCH_GROOVEDSPINES, RESEARCH_MUSCULARAUGMENTS, CANCEL_LAST
    | ZergLurkermp  -- SMART, MOVE, PATROL, HOLDPOSITION, BURROWDOWN, STOP, ATTACK
    | ZergLurkermpburrowed  -- SMART, BURROWUP, STOP, ATTACK, BEHAVIOR_HOLDFIREON, BEHAVIOR_HOLDFIREOFF
    | ZergLurkermpegg  -- SMART, CANCEL, RALLY_UNITS
    | ZergMutalisk  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ZergNyduscanal  -- SMART, UNLOADALL, STOP, LOAD, RALLY_UNITS
    | ZergNydusnetwork  -- SMART, BUILD_NYDUSWORM, CANCEL, UNLOADALL, STOP, LOAD, RALLY_UNITS
    | ZergOverlord  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_OVERSEER, BEHAVIOR_GENERATECREEPON, BEHAVIOR_GENERATECREEPOFF, MORPH_OVERLORDTRANSPORT, CANCEL, STOP, ATTACK
    | ZergOverlordcocoon  -- SMART, MOVE, PATROL, HOLDPOSITION, CANCEL
    | ZergOverlordtransport  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_OVERSEER, BEHAVIOR_GENERATECREEPON, BEHAVIOR_GENERATECREEPOFF, STOP, LOAD, UNLOADALLAT, ATTACK
    | ZergOverseer  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_SPAWNCHANGELING, EFFECT_CONTAMINATE, STOP, ATTACK
    | ZergQueen  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_INJECTLARVA, EFFECT_TRANSFUSION, BURROWDOWN, STOP, ATTACK, BUILD_CREEPTUMOR
    | ZergQueenburrowed  -- BURROWUP
    | ZergRavager  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_CORROSIVEBILE, BURROWDOWN, STOP, ATTACK
    | ZergRavagercocoon  -- SMART, CANCEL, RALLY_UNITS
    | ZergRoach  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_RAVAGER, BURROWDOWN, STOP, ATTACK
    | ZergRoachburrowed  -- SMART, MOVE, PATROL, HOLDPOSITION, BURROWUP, STOP, ATTACK
    | ZergRoachwarren  -- RESEARCH_GLIALREGENERATION, RESEARCH_TUNNELINGCLAWS, CANCEL, CANCEL_LAST
    | ZergSpawningpool  -- RESEARCH_ZERGLINGADRENALGLANDS, RESEARCH_ZERGLINGMETABOLICBOOST, CANCEL, CANCEL_LAST
    | ZergSpinecrawler  -- SMART, CANCEL, STOP, ATTACK, MORPH_UPROOT
    | ZergSpinecrawleruprooted  -- SMART, MOVE, PATROL, HOLDPOSITION, CANCEL, STOP, ATTACK, MORPH_ROOT
    | ZergSpire  -- MORPH_GREATERSPIRE, CANCEL, CANCEL_LAST, RESEARCH_ZERGFLYERARMOR, RESEARCH_ZERGFLYERATTACK
    | ZergSporecrawler  -- SMART, CANCEL, STOP, ATTACK, MORPH_UPROOT
    | ZergSporecrawleruprooted  -- SMART, MOVE, PATROL, HOLDPOSITION, CANCEL, STOP, ATTACK, MORPH_ROOT
    | ZergSwarmhostburrowedmp  -- SMART, EFFECT_SPAWNLOCUSTS, BURROWUP
    | ZergSwarmhostmp  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_SPAWNLOCUSTS, BURROWDOWN, STOP, ATTACK
    | ZergTransportoverlordcocoon  -- SMART, MOVE, PATROL, HOLDPOSITION, CANCEL
    | ZergUltralisk  -- SMART, MOVE, PATROL, HOLDPOSITION, BURROWDOWN, STOP, ATTACK
    | ZergUltraliskcavern  -- RESEARCH_CHITINOUSPLATING, CANCEL, CANCEL_LAST
    | ZergViper  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_BLINDINGCLOUD, EFFECT_ABDUCT, EFFECT_VIPERCONSUME, EFFECT_PARASITICBOMB, STOP, ATTACK
    | ZergZergling  -- SMART, MOVE, PATROL, HOLDPOSITION, TRAIN_BANELING, BURROWDOWN, STOP, ATTACK
    | ZergZerglingburrowed  -- BURROWUP
    | ZergParasiticbombdummy  -- 824,
    | ProtossAdept  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_ADEPTPHASESHIFT, CANCEL, STOP, RALLY_UNITS, ATTACK
    | ProtossAdeptphaseshift  -- SMART, MOVE, PATROL, HOLDPOSITION, CANCEL, STOP, ATTACK
    | ProtossArchon  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, RALLY_UNITS, ATTACK
    | ProtossAssimilator  -- CANCEL
    | ProtossAssimilatorrich  -- 1944,
    | ProtossCarrier  -- SMART, MOVE, PATROL, HOLDPOSITION, BUILD_INTERCEPTORS, STOP, CANCEL_LAST, ATTACK
    | ProtossColossus  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ProtossCyberneticscore  -- RESEARCH_WARPGATE, CANCEL, CANCEL_LAST, RESEARCH_PROTOSSAIRARMOR, RESEARCH_PROTOSSAIRWEAPONS
    | ProtossDarkshrine  -- RESEARCH_SHADOWSTRIKE, CANCEL, CANCEL_LAST
    | ProtossDarktemplar  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, RALLY_UNITS, ATTACK, EFFECT_BLINK
    | ProtossDisruptor  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_PURIFICATIONNOVA, STOP, ATTACK
    | ProtossDisruptorphased  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ProtossFleetbeacon  -- RESEARCH_INTERCEPTORGRAVITONCATAPULT, RESEARCH_PHOENIXANIONPULSECRYSTALS, CANCEL, CANCEL_LAST
    | ProtossForge  -- CANCEL, CANCEL_LAST, RESEARCH_PROTOSSGROUNDARMOR, RESEARCH_PROTOSSGROUNDWEAPONS, RESEARCH_PROTOSSSHIELDS
    | ProtossGateway  -- SMART, TRAIN_ZEALOT, TRAIN_STALKER, TRAIN_HIGHTEMPLAR, TRAIN_DARKTEMPLAR, TRAIN_SENTRY, TRAIN_ADEPT, MORPH_WARPGATE, CANCEL, CANCEL_LAST, RALLY_UNITS
    | ProtossHightemplar  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_FEEDBACK, EFFECT_PSISTORM, STOP, RALLY_UNITS, ATTACK
    | ProtossImmortal  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_IMMORTALBARRIER, STOP, ATTACK
    | ProtossInterceptor  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ProtossMothership  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_PHOTONOVERCHARGE, EFFECT_TIMEWARP, STOP, ATTACK, EFFECT_MASSRECALL
    | ProtossMothershipcore  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_MOTHERSHIP, EFFECT_PHOTONOVERCHARGE, EFFECT_TIMEWARP, CANCEL, STOP, ATTACK, EFFECT_MASSRECALL
    | ProtossNexus  -- SMART, EFFECT_CHRONOBOOST, TRAIN_PROBE, TRAIN_MOTHERSHIP, CANCEL, CANCEL_LAST, RALLY_WORKERS
    | ProtossObserver  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, ATTACK
    | ProtossOracle  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_ORACLEREVELATION, BEHAVIOR_PULSARBEAMON, BEHAVIOR_PULSARBEAMOFF, BUILD_STASISTRAP, CANCEL, STOP, ATTACK
    | ProtossOraclestasistrap  -- CANCEL
    | ProtossPhoenix  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_GRAVITONBEAM, CANCEL, STOP, ATTACK
    | ProtossPhotoncannon  -- SMART, CANCEL, STOP, ATTACK
    | ProtossProbe  -- SMART, MOVE, PATROL, HOLDPOSITION, BUILD_NEXUS, BUILD_PYLON, BUILD_ASSIMILATOR, BUILD_GATEWAY, BUILD_FORGE, BUILD_FLEETBEACON, BUILD_TWILIGHTCOUNCIL, BUILD_PHOTONCANNON, BUILD_SHIELDBATTERY, BUILD_STARGATE, BUILD_TEMPLARARCHIVE, BUILD_DARKSHRINE, BUILD_ROBOTICSBAY, BUILD_ROBOTICSFACILITY, BUILD_CYBERNETICSCORE, STOP, HARVEST_GATHER, HARVEST_RETURN, ATTACK, EFFECT_SPRAY
    | ProtossPylon  -- CANCEL
    | ProtossPylonovercharged  -- SMART, STOP, ATTACK
    | ProtossRoboticsbay  -- RESEARCH_GRAVITICBOOSTER, RESEARCH_GRAVITICDRIVE, RESEARCH_EXTENDEDTHERMALLANCE, CANCEL, CANCEL_LAST
    | ProtossRoboticsfacility  -- SMART, TRAIN_WARPPRISM, TRAIN_OBSERVER, TRAIN_COLOSSUS, TRAIN_IMMORTAL, TRAIN_DISRUPTOR, CANCEL, CANCEL_LAST, RALLY_UNITS
    | ProtossSentry  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_GUARDIANSHIELD, HALLUCINATION_ARCHON, HALLUCINATION_COLOSSUS, HALLUCINATION_HIGHTEMPLAR, HALLUCINATION_IMMORTAL, HALLUCINATION_PHOENIX, HALLUCINATION_PROBE, HALLUCINATION_STALKER, HALLUCINATION_VOIDRAY, HALLUCINATION_WARPPRISM, HALLUCINATION_ZEALOT, EFFECT_FORCEFIELD, HALLUCINATION_ORACLE, HALLUCINATION_DISRUPTOR, HALLUCINATION_ADEPT, STOP, RALLY_UNITS, ATTACK
    | ProtossShieldbattery  -- SMART, EFFECT_RESTORE
    | ProtossStalker  -- SMART, MOVE, PATROL, HOLDPOSITION, STOP, RALLY_UNITS, ATTACK, EFFECT_BLINK
    | ProtossStargate  -- SMART, TRAIN_PHOENIX, TRAIN_CARRIER, TRAIN_VOIDRAY, TRAIN_ORACLE, TRAIN_TEMPEST, CANCEL, CANCEL_LAST, RALLY_UNITS
    | ProtossTempest  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_TEMPESTDISRUPTIONBLAST, CANCEL, STOP, ATTACK
    | ProtossTemplararchive  -- RESEARCH_PSISTORM, CANCEL, CANCEL_LAST
    | ProtossTwilightcouncil  -- RESEARCH_CHARGE, RESEARCH_BLINK, RESEARCH_ADEPTRESONATINGGLAIVES, CANCEL, CANCEL_LAST
    | ProtossVoidray  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_VOIDRAYPRISMATICALIGNMENT, STOP, ATTACK
    | ProtossWarpgate  -- SMART, TRAINWARP_ZEALOT, TRAINWARP_STALKER, TRAINWARP_HIGHTEMPLAR, TRAINWARP_DARKTEMPLAR, TRAINWARP_SENTRY, TRAINWARP_ADEPT, MORPH_GATEWAY
    | ProtossWarpprism  -- SMART, MOVE, PATROL, HOLDPOSITION, MORPH_WARPPRISMPHASINGMODE, STOP, LOAD, UNLOADALLAT, ATTACK
    | ProtossWarpprismphasing  -- SMART, MORPH_WARPPRISMTRANSPORTMODE, STOP, LOAD, UNLOADALLAT
    | ProtossZealot  -- SMART, MOVE, PATROL, HOLDPOSITION, EFFECT_CHARGE, STOP, RALLY_UNITS, ATTACK
    | NeutralBattlestationmineralfield  -- 886,
    | NeutralBattlestationmineralfield750  -- 887,
    | NeutralCollapsiblerocktowerdebris  -- 490,
    | NeutralCollapsiblerocktowerdiagonal  -- 588,
    | NeutralCollapsiblerocktowerpushunit  -- 561,
    | NeutralCollapsibleterrantowerdebris  -- 485,
    | NeutralCollapsibleterrantowerdiagonal  -- 589,
    | NeutralCollapsibleterrantowerpushunit  -- 562,
    | NeutralCollapsibleterrantowerpushunitrampleft  -- 559,
    | NeutralCollapsibleterrantowerpushunitrampright  -- 560,
    | NeutralCollapsibleterrantowerrampleft  -- 590,
    | NeutralCollapsibleterrantowerrampright  -- 591,
    | NeutralDebrisrampleft  -- 486,
    | NeutralDebrisrampright  -- 487,
    | NeutralDestructibledebris6x6  -- 365,
    | NeutralDestructibledebrisrampdiagonalhugeblur  -- 377,
    | NeutralDestructibledebrisrampdiagonalhugeulbr  -- 376,
    | NeutralDestructiblerock6x6  -- 371,
    | NeutralDestructiblerockex1diagonalhugeblur  -- 641,
    | NeutralForcefield  -- 135,
    | NeutralKarakfemale  -- 324,
    | NeutralLabmineralfield  -- 665,
    | NeutralLabmineralfield750  -- 666,
    | NeutralMineralfield  -- 341,
    | NeutralMineralfield750  -- 483,
    | NeutralProtossvespenegeyser  -- 608,
    | NeutralPurifiermineralfield  -- 884,
    | NeutralPurifiermineralfield750  -- 885,
    | NeutralPurifierrichmineralfield  -- 796,
    | NeutralPurifierrichmineralfield750  -- 797,
    | NeutralPurifiervespenegeyser  -- 880,
    | NeutralRichmineralfield  -- 146,
    | NeutralRichmineralfield750  -- 147,
    | NeutralRichvespenegeyser  -- 344,
    | NeutralScantipede  -- 335,
    | NeutralShakurasvespenegeyser  -- 881,
    | NeutralSpaceplatformgeyser  -- 343,
    | NeutralUnbuildablebricksdestructible  -- 473,
    | NeutralUnbuildableplatesdestructible  -- 474,
    | NeutralUtilitybot  -- 330,
    | NeutralVespenegeyser  -- 342,
    | NeutralXelnagatower  -- 149,
    deriving (Show, Eq) --Enum

instance Enum UnitTypeId where
  fromEnum :: UnitTypeId -> Int
  fromEnum x = case x of
      Invalid -> 0
      TerranArmory -> 29
      TerranAutoturret -> 31
      TerranBanshee -> 55
      TerranBarracks -> 21
      TerranBarracksflying -> 46
      TerranBarracksreactor -> 38
      TerranBarrackstechlab -> 37
      TerranBattlecruiser -> 57
      TerranBunker -> 24
      TerranCommandcenter -> 18
      TerranCommandcenterflying -> 36
      TerranCyclone -> 692
      TerranEngineeringbay -> 22
      TerranFactory -> 27
      TerranFactoryflying -> 43
      TerranFactoryreactor -> 40
      TerranFactorytechlab -> 39
      TerranFusioncore -> 30
      TerranGhost -> 50
      TerranGhostacademy -> 26
      TerranHellion -> 53
      TerranHelliontank -> 484
      TerranLiberator -> 689
      TerranLiberatorag -> 734
      TerranMarauder -> 51
      TerranMarine -> 48
      TerranMedivac -> 54
      TerranMissileturret -> 23
      TerranMule -> 268
      TerranOrbitalcommand -> 132
      TerranOrbitalcommandflying -> 134
      TerranPlanetaryfortress -> 130
      TerranRaven -> 56
      TerranReaper -> 49
      TerranRefinery -> 20
      TerranRefineryrich -> 1949
      TerranScv -> 45
      TerranSensortower -> 25
      TerranSiegetank -> 33
      TerranSiegetanksieged -> 32
      TerranStarport -> 28
      TerranStarportflying -> 44
      TerranStarportreactor -> 42
      TerranStarporttechlab -> 41
      TerranSupplydepot -> 19
      TerranSupplydepotlowered -> 47
      TerranThor -> 52
      TerranThorap -> 691
      TerranVikingassault -> 34
      TerranVikingfighter -> 35
      TerranWidowmine -> 498
      TerranWidowmineburrowed -> 500
      TerranKd8charge -> 830
      TerranNuke -> 58
      TerranPointdefensedrone -> 11
      TerranReactor -> 6
      TerranTechlab -> 5
      ZergBaneling -> 9
      ZergBanelingburrowed -> 115
      ZergBanelingcocoon -> 8
      ZergBanelingnest -> 96
      ZergBroodling -> 289
      ZergBroodlord -> 114
      ZergBroodlordcocoon -> 113
      ZergChangeling -> 12
      ZergChangelingmarine -> 15
      ZergChangelingmarineshield -> 14
      ZergChangelingzealot -> 13
      ZergChangelingzergling -> 17
      ZergChangelingzerglingwings -> 16
      ZergCorruptor -> 112
      ZergCreeptumor -> 87
      ZergCreeptumorburrowed -> 137
      ZergCreeptumorqueen -> 138
      ZergDrone -> 104
      ZergDroneburrowed -> 116
      ZergEgg -> 103
      ZergEvolutionchamber -> 90
      ZergExtractor -> 88
      ZergExtractorrich -> 1945
      ZergGreaterspire -> 102
      ZergHatchery -> 86
      ZergHive -> 101
      ZergHydralisk -> 107
      ZergHydraliskburrowed -> 117
      ZergHydraliskden -> 91
      ZergInfestationpit -> 94
      ZergInfestedterransegg -> 150
      ZergInfestor -> 111
      ZergInfestorburrowed -> 127
      ZergInfestorterran -> 7
      ZergLair -> 100
      ZergLarva -> 151
      ZergLocustmp -> 489
      ZergLocustmpflying -> 693
      ZergLurkerdenmp -> 504
      ZergLurkermp -> 502
      ZergLurkermpburrowed -> 503
      ZergLurkermpegg -> 501
      ZergMutalisk -> 108
      ZergNyduscanal -> 142
      ZergNydusnetwork -> 95
      ZergOverlord -> 106
      ZergOverlordcocoon -> 128
      ZergOverlordtransport -> 893
      ZergOverseer -> 129
      ZergQueen -> 126
      ZergQueenburrowed -> 125
      ZergRavager -> 688
      ZergRavagercocoon -> 687
      ZergRoach -> 110
      ZergRoachburrowed -> 118
      ZergRoachwarren -> 97
      ZergSpawningpool -> 89
      ZergSpinecrawler -> 98
      ZergSpinecrawleruprooted -> 139
      ZergSpire -> 92
      ZergSporecrawler -> 99
      ZergSporecrawleruprooted -> 140
      ZergSwarmhostburrowedmp -> 493
      ZergSwarmhostmp -> 494
      ZergTransportoverlordcocoon -> 892
      ZergUltralisk -> 109
      ZergUltraliskcavern -> 93
      ZergViper -> 499
      ZergZergling -> 105
      ZergZerglingburrowed -> 119
      ZergParasiticbombdummy -> 824
      ProtossAdept -> 311
      ProtossAdeptphaseshift -> 801
      ProtossArchon -> 141
      ProtossAssimilator -> 61
      ProtossAssimilatorrich -> 1944
      ProtossCarrier -> 79
      ProtossColossus -> 4
      ProtossCyberneticscore -> 72
      ProtossDarkshrine -> 69
      ProtossDarktemplar -> 76
      ProtossDisruptor -> 694
      ProtossDisruptorphased -> 733
      ProtossFleetbeacon -> 64
      ProtossForge -> 63
      ProtossGateway -> 62
      ProtossHightemplar -> 75
      ProtossImmortal -> 83
      ProtossInterceptor -> 85
      ProtossMothership -> 10
      ProtossMothershipcore -> 488
      ProtossNexus -> 59
      ProtossObserver -> 82
      ProtossOracle -> 495
      ProtossOraclestasistrap -> 732
      ProtossPhoenix -> 78
      ProtossPhotoncannon -> 66
      ProtossProbe -> 84
      ProtossPylon -> 60
      ProtossPylonovercharged -> 894
      ProtossRoboticsbay -> 70
      ProtossRoboticsfacility -> 71
      ProtossSentry -> 77
      ProtossShieldbattery -> 1910
      ProtossStalker -> 74
      ProtossStargate -> 67
      ProtossTempest -> 496
      ProtossTemplararchive -> 68
      ProtossTwilightcouncil -> 65
      ProtossVoidray -> 80
      ProtossWarpgate -> 133
      ProtossWarpprism -> 81
      ProtossWarpprismphasing -> 136
      ProtossZealot -> 73
      NeutralBattlestationmineralfield -> 886
      NeutralBattlestationmineralfield750 -> 887
      NeutralCollapsiblerocktowerdebris -> 490
      NeutralCollapsiblerocktowerdiagonal -> 588
      NeutralCollapsiblerocktowerpushunit -> 561
      NeutralCollapsibleterrantowerdebris -> 485
      NeutralCollapsibleterrantowerdiagonal -> 589
      NeutralCollapsibleterrantowerpushunit -> 562
      NeutralCollapsibleterrantowerpushunitrampleft -> 559
      NeutralCollapsibleterrantowerpushunitrampright -> 560
      NeutralCollapsibleterrantowerrampleft -> 590
      NeutralCollapsibleterrantowerrampright -> 591
      NeutralDebrisrampleft -> 486
      NeutralDebrisrampright -> 487
      NeutralDestructibledebris6x6 -> 365
      NeutralDestructibledebrisrampdiagonalhugeblur -> 377
      NeutralDestructibledebrisrampdiagonalhugeulbr -> 376
      NeutralDestructiblerock6x6 -> 371
      NeutralDestructiblerockex1diagonalhugeblur -> 641
      NeutralForcefield -> 135
      NeutralKarakfemale -> 324
      NeutralLabmineralfield -> 665
      NeutralLabmineralfield750 -> 666
      NeutralMineralfield -> 341
      NeutralMineralfield750 -> 483
      NeutralProtossvespenegeyser -> 608
      NeutralPurifiermineralfield -> 884
      NeutralPurifiermineralfield750 -> 885
      NeutralPurifierrichmineralfield -> 796
      NeutralPurifierrichmineralfield750 -> 797
      NeutralPurifiervespenegeyser -> 880
      NeutralRichmineralfield -> 146
      NeutralRichmineralfield750 -> 147
      NeutralRichvespenegeyser -> 344
      NeutralScantipede -> 335
      NeutralShakurasvespenegeyser -> 881
      NeutralSpaceplatformgeyser -> 343
      NeutralUnbuildablebricksdestructible -> 473
      NeutralUnbuildableplatesdestructible -> 474
      NeutralUtilitybot -> 330
      NeutralVespenegeyser -> 342
      NeutralXelnagatower -> 149

  toEnum :: Int -> UnitTypeId
  toEnum x = case x of
      29 -> TerranArmory
      31 -> TerranAutoturret
      55 -> TerranBanshee
      21 -> TerranBarracks
      46 -> TerranBarracksflying
      38 -> TerranBarracksreactor
      37 -> TerranBarrackstechlab
      57 -> TerranBattlecruiser
      24 -> TerranBunker
      18 -> TerranCommandcenter
      36 -> TerranCommandcenterflying
      692 -> TerranCyclone
      22 -> TerranEngineeringbay
      27 -> TerranFactory
      43 -> TerranFactoryflying
      40 -> TerranFactoryreactor
      39 -> TerranFactorytechlab
      30 -> TerranFusioncore
      50 -> TerranGhost
      26 -> TerranGhostacademy
      53 -> TerranHellion
      484 -> TerranHelliontank
      689 -> TerranLiberator
      734 -> TerranLiberatorag
      51 -> TerranMarauder
      48 -> TerranMarine
      54 -> TerranMedivac
      23 -> TerranMissileturret
      268 -> TerranMule
      132 -> TerranOrbitalcommand
      134 -> TerranOrbitalcommandflying
      130 -> TerranPlanetaryfortress
      56 -> TerranRaven
      49 -> TerranReaper
      20 -> TerranRefinery
      1949 -> TerranRefineryrich
      45 -> TerranScv
      25 -> TerranSensortower
      33 -> TerranSiegetank
      32 -> TerranSiegetanksieged
      28 -> TerranStarport
      44 -> TerranStarportflying
      42 -> TerranStarportreactor
      41 -> TerranStarporttechlab
      19 -> TerranSupplydepot
      47 -> TerranSupplydepotlowered
      52 -> TerranThor
      691 -> TerranThorap
      34 -> TerranVikingassault
      35 -> TerranVikingfighter
      498 -> TerranWidowmine
      500 -> TerranWidowmineburrowed
      830 -> TerranKd8charge
      58 -> TerranNuke
      11 -> TerranPointdefensedrone
      6 -> TerranReactor
      5 -> TerranTechlab
      9 -> ZergBaneling
      115 -> ZergBanelingburrowed
      8 -> ZergBanelingcocoon
      96 -> ZergBanelingnest
      289 -> ZergBroodling
      114 -> ZergBroodlord
      113 -> ZergBroodlordcocoon
      12 -> ZergChangeling
      15 -> ZergChangelingmarine
      14 -> ZergChangelingmarineshield
      13 -> ZergChangelingzealot
      17 -> ZergChangelingzergling
      16 -> ZergChangelingzerglingwings
      112 -> ZergCorruptor
      87 -> ZergCreeptumor
      137 -> ZergCreeptumorburrowed
      138 -> ZergCreeptumorqueen
      104 -> ZergDrone
      116 -> ZergDroneburrowed
      103 -> ZergEgg
      90 -> ZergEvolutionchamber
      88 -> ZergExtractor
      1945 -> ZergExtractorrich
      102 -> ZergGreaterspire
      86 -> ZergHatchery
      101 -> ZergHive
      107 -> ZergHydralisk
      117 -> ZergHydraliskburrowed
      91 -> ZergHydraliskden
      94 -> ZergInfestationpit
      150 -> ZergInfestedterransegg
      111 -> ZergInfestor
      127 -> ZergInfestorburrowed
      7 -> ZergInfestorterran
      100 -> ZergLair
      151 -> ZergLarva
      489 -> ZergLocustmp
      693 -> ZergLocustmpflying
      504 -> ZergLurkerdenmp
      502 -> ZergLurkermp
      503 -> ZergLurkermpburrowed
      501 -> ZergLurkermpegg
      108 -> ZergMutalisk
      142 -> ZergNyduscanal
      95 -> ZergNydusnetwork
      106 -> ZergOverlord
      128 -> ZergOverlordcocoon
      893 -> ZergOverlordtransport
      129 -> ZergOverseer
      126 -> ZergQueen
      125 -> ZergQueenburrowed
      688 -> ZergRavager
      687 -> ZergRavagercocoon
      110 -> ZergRoach
      118 -> ZergRoachburrowed
      97 -> ZergRoachwarren
      89 -> ZergSpawningpool
      98 -> ZergSpinecrawler
      139 -> ZergSpinecrawleruprooted
      92 -> ZergSpire
      99 -> ZergSporecrawler
      140 -> ZergSporecrawleruprooted
      493 -> ZergSwarmhostburrowedmp
      494 -> ZergSwarmhostmp
      892 -> ZergTransportoverlordcocoon
      109 -> ZergUltralisk
      93 -> ZergUltraliskcavern
      499 -> ZergViper
      105 -> ZergZergling
      119 -> ZergZerglingburrowed
      824 -> ZergParasiticbombdummy
      311 -> ProtossAdept
      801 -> ProtossAdeptphaseshift
      141 -> ProtossArchon
      61 -> ProtossAssimilator
      1944 -> ProtossAssimilatorrich
      79 -> ProtossCarrier
      4 -> ProtossColossus
      72 -> ProtossCyberneticscore
      69 -> ProtossDarkshrine
      76 -> ProtossDarktemplar
      694 -> ProtossDisruptor
      733 -> ProtossDisruptorphased
      64 -> ProtossFleetbeacon
      63 -> ProtossForge
      62 -> ProtossGateway
      75 -> ProtossHightemplar
      83 -> ProtossImmortal
      85 -> ProtossInterceptor
      10 -> ProtossMothership
      488 -> ProtossMothershipcore
      59 -> ProtossNexus
      82 -> ProtossObserver
      495 -> ProtossOracle
      732 -> ProtossOraclestasistrap
      78 -> ProtossPhoenix
      66 -> ProtossPhotoncannon
      84 -> ProtossProbe
      60 -> ProtossPylon
      894 -> ProtossPylonovercharged
      70 -> ProtossRoboticsbay
      71 -> ProtossRoboticsfacility
      77 -> ProtossSentry
      1910 -> ProtossShieldbattery
      74 -> ProtossStalker
      67 -> ProtossStargate
      496 -> ProtossTempest
      68 -> ProtossTemplararchive
      65 -> ProtossTwilightcouncil
      80 -> ProtossVoidray
      133 -> ProtossWarpgate
      81 -> ProtossWarpprism
      136 -> ProtossWarpprismphasing
      73 -> ProtossZealot
      886 -> NeutralBattlestationmineralfield
      887 -> NeutralBattlestationmineralfield750
      490 -> NeutralCollapsiblerocktowerdebris
      588 -> NeutralCollapsiblerocktowerdiagonal
      561 -> NeutralCollapsiblerocktowerpushunit
      485 -> NeutralCollapsibleterrantowerdebris
      589 -> NeutralCollapsibleterrantowerdiagonal
      562 -> NeutralCollapsibleterrantowerpushunit
      559 -> NeutralCollapsibleterrantowerpushunitrampleft
      560 -> NeutralCollapsibleterrantowerpushunitrampright
      590 -> NeutralCollapsibleterrantowerrampleft
      591 -> NeutralCollapsibleterrantowerrampright
      486 -> NeutralDebrisrampleft
      487 -> NeutralDebrisrampright
      365 -> NeutralDestructibledebris6x6
      377 -> NeutralDestructibledebrisrampdiagonalhugeblur
      376 -> NeutralDestructibledebrisrampdiagonalhugeulbr
      371 -> NeutralDestructiblerock6x6
      641 -> NeutralDestructiblerockex1diagonalhugeblur
      135 -> NeutralForcefield
      324 -> NeutralKarakfemale
      665 -> NeutralLabmineralfield
      666 -> NeutralLabmineralfield750
      341 -> NeutralMineralfield
      483 -> NeutralMineralfield750
      608 -> NeutralProtossvespenegeyser
      884 -> NeutralPurifiermineralfield
      885 -> NeutralPurifiermineralfield750
      796 -> NeutralPurifierrichmineralfield
      797 -> NeutralPurifierrichmineralfield750
      880 -> NeutralPurifiervespenegeyser
      146 -> NeutralRichmineralfield
      147 -> NeutralRichmineralfield750
      344 -> NeutralRichvespenegeyser
      335 -> NeutralScantipede
      881 -> NeutralShakurasvespenegeyser
      343 -> NeutralSpaceplatformgeyser
      473 -> NeutralUnbuildablebricksdestructible
      474 -> NeutralUnbuildableplatesdestructible
      330 -> NeutralUtilitybot
      342 -> NeutralVespenegeyser
      149 -> NeutralXelnagatower
      _ -> Invalid