
module SC2.Ids.UpgradeId(UpgradeId(..)) where

data UpgradeId =
   Invalid  -- 0
 | Carrierlaunchspeedupgrade  -- 1
 | Glialreconstitution  -- 2
 | Tunnelingclaws  -- 3
 | Chitinousplating  -- 4
 | Hisecautotracking  -- 5
 | Terranbuildingarmor  -- 6
 | Terraninfantryweaponslevel1  -- 7
 | Terraninfantryweaponslevel2  -- 8
 | Terraninfantryweaponslevel3  -- 9
 | Neosteelframe  -- 10
 | Terraninfantryarmorslevel1  -- 11
 | Terraninfantryarmorslevel2  -- 12
 | Terraninfantryarmorslevel3  -- 13
 | Stimpack  -- 15
 | Shieldwall  -- 16
 | Punishergrenades  -- 17
 | Highcapacitybarrels  -- 19
 | Bansheecloak  -- 20
 | Ravencorvidreactor  -- 22
 | Personalcloaking  -- 25
 | Terranvehicleweaponslevel1  -- 30
 | Terranvehicleweaponslevel2  -- 31
 | Terranvehicleweaponslevel3  -- 32
 | Terranshipweaponslevel1  -- 36
 | Terranshipweaponslevel2  -- 37
 | Terranshipweaponslevel3  -- 38
 | Protossgroundweaponslevel1  -- 39
 | Protossgroundweaponslevel2  -- 40
 | Protossgroundweaponslevel3  -- 41
 | Protossgroundarmorslevel1  -- 42
 | Protossgroundarmorslevel2  -- 43
 | Protossgroundarmorslevel3  -- 44
 | Protossshieldslevel1  -- 45
 | Protossshieldslevel2  -- 46
 | Protossshieldslevel3  -- 47
 | Observergraviticbooster  -- 48
 | Graviticdrive  -- 49
 | Extendedthermallance  -- 50
 | Psistormtech  -- 52
 | Zergmeleeweaponslevel1  -- 53
 | Zergmeleeweaponslevel2  -- 54
 | Zergmeleeweaponslevel3  -- 55
 | Zerggroundarmorslevel1  -- 56
 | Zerggroundarmorslevel2  -- 57
 | Zerggroundarmorslevel3  -- 58
 | Zergmissileweaponslevel1  -- 59
 | Zergmissileweaponslevel2  -- 60
 | Zergmissileweaponslevel3  -- 61
 | Overlordspeed  -- 62
 | Burrow  -- 64
 | Zerglingattackspeed  -- 65
 | Zerglingmovementspeed  -- 66
 | Zergflyerweaponslevel1  -- 68
 | Zergflyerweaponslevel2  -- 69
 | Zergflyerweaponslevel3  -- 70
 | Zergflyerarmorslevel1  -- 71
 | Zergflyerarmorslevel2  -- 72
 | Zergflyerarmorslevel3  -- 73
 | Infestorenergyupgrade  -- 74
 | Centrificalhooks  -- 75
 | Battlecruiserenablespecializations  -- 76
 | Protossairweaponslevel1  -- 78
 | Protossairweaponslevel2  -- 79
 | Protossairweaponslevel3  -- 80
 | Protossairarmorslevel1  -- 81
 | Protossairarmorslevel2  -- 82
 | Protossairarmorslevel3  -- 83
 | Warpgateresearch  -- 84
 | Charge  -- 86
 | Blinktech  -- 87
 | Phoenixrangeupgrade  -- 99
 | Neuralparasite  -- 101
 | Terranvehicleandshiparmorslevel1  -- 116
 | Terranvehicleandshiparmorslevel2  -- 117
 | Terranvehicleandshiparmorslevel3  -- 118
 | Drillclaws  -- 122
 | Adeptpiercingattack  -- 130
 | Magfieldlaunchers  -- 133
 | Evolvegroovedspines  -- 134
 | Evolvemuscularaugments  -- 135
 | Bansheespeed  -- 136
 | Ravenrecalibratedexplosives  -- 138
 | Medivacincreasespeedboost  -- 139
 | Liberatoragrangeupgrade  -- 140
 | Darktemplarblinkupgrade  -- 141
 | Cyclonelockondamageupgrade  -- 144
 | Smartservos  -- 289
 | Rapidfirelaunchers  -- 291
 | Enhancedmunitions  -- 292
 deriving (Show, Eq)

instance Enum UpgradeId where

  --fromEnum :: UpgradeId -> Int
  fromEnum x = case x of
    Invalid -> 0
    Carrierlaunchspeedupgrade -> 1
    Glialreconstitution -> 2
    Tunnelingclaws -> 3
    Chitinousplating -> 4
    Hisecautotracking -> 5
    Terranbuildingarmor -> 6
    Terraninfantryweaponslevel1 -> 7
    Terraninfantryweaponslevel2 -> 8
    Terraninfantryweaponslevel3 -> 9
    Neosteelframe -> 10
    Terraninfantryarmorslevel1 -> 11
    Terraninfantryarmorslevel2 -> 12
    Terraninfantryarmorslevel3 -> 13
    Stimpack -> 15
    Shieldwall -> 16
    Punishergrenades -> 17
    Highcapacitybarrels -> 19
    Bansheecloak -> 20
    Ravencorvidreactor -> 22
    Personalcloaking -> 25
    Terranvehicleweaponslevel1 -> 30
    Terranvehicleweaponslevel2 -> 31
    Terranvehicleweaponslevel3 -> 32
    Terranshipweaponslevel1 -> 36
    Terranshipweaponslevel2 -> 37
    Terranshipweaponslevel3 -> 38
    Protossgroundweaponslevel1 -> 39
    Protossgroundweaponslevel2 -> 40
    Protossgroundweaponslevel3 -> 41
    Protossgroundarmorslevel1 -> 42
    Protossgroundarmorslevel2 -> 43
    Protossgroundarmorslevel3 -> 44
    Protossshieldslevel1 -> 45
    Protossshieldslevel2 -> 46
    Protossshieldslevel3 -> 47
    Observergraviticbooster -> 48
    Graviticdrive -> 49
    Extendedthermallance -> 50
    Psistormtech -> 52
    Zergmeleeweaponslevel1 -> 53
    Zergmeleeweaponslevel2 -> 54
    Zergmeleeweaponslevel3 -> 55
    Zerggroundarmorslevel1 -> 56
    Zerggroundarmorslevel2 -> 57
    Zerggroundarmorslevel3 -> 58
    Zergmissileweaponslevel1 -> 59
    Zergmissileweaponslevel2 -> 60
    Zergmissileweaponslevel3 -> 61
    Overlordspeed -> 62
    Burrow -> 64
    Zerglingattackspeed -> 65
    Zerglingmovementspeed -> 66
    Zergflyerweaponslevel1 -> 68
    Zergflyerweaponslevel2 -> 69
    Zergflyerweaponslevel3 -> 70
    Zergflyerarmorslevel1 -> 71
    Zergflyerarmorslevel2 -> 72
    Zergflyerarmorslevel3 -> 73
    Infestorenergyupgrade -> 74
    Centrificalhooks -> 75
    Battlecruiserenablespecializations -> 76
    Protossairweaponslevel1 -> 78
    Protossairweaponslevel2 -> 79
    Protossairweaponslevel3 -> 80
    Protossairarmorslevel1 -> 81
    Protossairarmorslevel2 -> 82
    Protossairarmorslevel3 -> 83
    Warpgateresearch -> 84
    Charge -> 86
    Blinktech -> 87
    Phoenixrangeupgrade -> 99
    Neuralparasite -> 101
    Terranvehicleandshiparmorslevel1 -> 116
    Terranvehicleandshiparmorslevel2 -> 117
    Terranvehicleandshiparmorslevel3 -> 118
    Drillclaws -> 122
    Adeptpiercingattack -> 130
    Magfieldlaunchers -> 133
    Evolvegroovedspines -> 134
    Evolvemuscularaugments -> 135
    Bansheespeed -> 136
    Ravenrecalibratedexplosives -> 138
    Medivacincreasespeedboost -> 139
    Liberatoragrangeupgrade -> 140
    Darktemplarblinkupgrade -> 141
    Cyclonelockondamageupgrade -> 144
    Smartservos -> 289
    Rapidfirelaunchers -> 291
    Enhancedmunitions -> 292

  --toEnum :: Int -> UpgradeId
  toEnum x = case x of
    0 -> Invalid
    1 -> Carrierlaunchspeedupgrade
    2 -> Glialreconstitution
    3 -> Tunnelingclaws
    4 -> Chitinousplating
    5 -> Hisecautotracking
    6 -> Terranbuildingarmor
    7 -> Terraninfantryweaponslevel1
    8 -> Terraninfantryweaponslevel2
    9 -> Terraninfantryweaponslevel3
    10 -> Neosteelframe
    11 -> Terraninfantryarmorslevel1
    12 -> Terraninfantryarmorslevel2
    13 -> Terraninfantryarmorslevel3
    15 -> Stimpack
    16 -> Shieldwall
    17 -> Punishergrenades
    19 -> Highcapacitybarrels
    20 -> Bansheecloak
    22 -> Ravencorvidreactor
    25 -> Personalcloaking
    30 -> Terranvehicleweaponslevel1
    31 -> Terranvehicleweaponslevel2
    32 -> Terranvehicleweaponslevel3
    36 -> Terranshipweaponslevel1
    37 -> Terranshipweaponslevel2
    38 -> Terranshipweaponslevel3
    39 -> Protossgroundweaponslevel1
    40 -> Protossgroundweaponslevel2
    41 -> Protossgroundweaponslevel3
    42 -> Protossgroundarmorslevel1
    43 -> Protossgroundarmorslevel2
    44 -> Protossgroundarmorslevel3
    45 -> Protossshieldslevel1
    46 -> Protossshieldslevel2
    47 -> Protossshieldslevel3
    48 -> Observergraviticbooster
    49 -> Graviticdrive
    50 -> Extendedthermallance
    52 -> Psistormtech
    53 -> Zergmeleeweaponslevel1
    54 -> Zergmeleeweaponslevel2
    55 -> Zergmeleeweaponslevel3
    56 -> Zerggroundarmorslevel1
    57 -> Zerggroundarmorslevel2
    58 -> Zerggroundarmorslevel3
    59 -> Zergmissileweaponslevel1
    60 -> Zergmissileweaponslevel2
    61 -> Zergmissileweaponslevel3
    62 -> Overlordspeed
    64 -> Burrow
    65 -> Zerglingattackspeed
    66 -> Zerglingmovementspeed
    68 -> Zergflyerweaponslevel1
    69 -> Zergflyerweaponslevel2
    70 -> Zergflyerweaponslevel3
    71 -> Zergflyerarmorslevel1
    72 -> Zergflyerarmorslevel2
    73 -> Zergflyerarmorslevel3
    74 -> Infestorenergyupgrade
    75 -> Centrificalhooks
    76 -> Battlecruiserenablespecializations
    78 -> Protossairweaponslevel1
    79 -> Protossairweaponslevel2
    80 -> Protossairweaponslevel3
    81 -> Protossairarmorslevel1
    82 -> Protossairarmorslevel2
    83 -> Protossairarmorslevel3
    84 -> Warpgateresearch
    86 -> Charge
    87 -> Blinktech
    99 -> Phoenixrangeupgrade
    101 -> Neuralparasite
    116 -> Terranvehicleandshiparmorslevel1
    117 -> Terranvehicleandshiparmorslevel2
    118 -> Terranvehicleandshiparmorslevel3
    122 -> Drillclaws
    130 -> Adeptpiercingattack
    133 -> Magfieldlaunchers
    134 -> Evolvegroovedspines
    135 -> Evolvemuscularaugments
    136 -> Bansheespeed
    138 -> Ravenrecalibratedexplosives
    139 -> Medivacincreasespeedboost
    140 -> Liberatoragrangeupgrade
    141 -> Darktemplarblinkupgrade
    144 -> Cyclonelockondamageupgrade
    289 -> Smartservos
    291 -> Rapidfirelaunchers
    292 -> Enhancedmunitions
    _ -> Invalid
