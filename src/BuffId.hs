
module BuffId (BuffId(..)) where

data BuffId =
   Invalid  -- 0
 | Gravitonbeam  -- 5
 | Ghostcloak  -- 6
 | Bansheecloak  -- 7
 | Poweruserwarpable  -- 8
 | Queenspawnlarvatimer  -- 11
 | Ghostholdfire  -- 12
 | Ghostholdfireb  -- 13
 | Empdecloak  -- 16
 | Fungalgrowth  -- 17
 | Guardianshield  -- 18
 | Timewarpproduction  -- 20
 | Neuralparasite  -- 22
 | Stimpackmarauder  -- 24
 | Supplydrop  -- 25
 | Stimpack  -- 27
 | Psistorm  -- 28
 | Cloakfieldeffect  -- 29
 | Charging  -- 30
 | Slow  -- 33
 | Contaminated  -- 36
 | Blindingcloudstructure  -- 38
 | Oraclerevelation  -- 49
 | Viperconsumestructure  -- 59
 | Blindingcloud  -- 83
 | Medivacspeedboost  -- 89
 | Purify  -- 97
 | Oracleweapon  -- 99
 | Immortaloverload  -- 102
 | Lockon  -- 116
 | Seekermissile  -- 120
 | Temporalfield  -- 121
 | Voidrayswarmdamageboost  -- 122
 | Oraclestasistraptarget  -- 129
 | Parasiticbomb  -- 132
 | Parasiticbombunitku  -- 133
 | Parasiticbombsecondaryunitsearch  -- 134
 | Lurkerholdfireb  -- 137
 | Channelsnipecombat  -- 145
 | Tempestdisruptionblaststunbehavior  -- 146
 | Carrymineralfieldminerals  -- 271
 | Carryhighyieldmineralfieldminerals  -- 272
 | Carryharvestablevespenegeysergas  -- 273
 | Carryharvestablevespenegeysergasprotoss  -- 274
 | Carryharvestablevespenegeysergaszerg  -- 275
 | Chronoboosted  -- 281
 deriving (Show, Eq)

instance Enum BuffId where

  --fromEnum :: BuffId -> Int
  fromEnum x = case x of
    Invalid -> 0
    Gravitonbeam -> 5
    Ghostcloak -> 6
    Bansheecloak -> 7
    Poweruserwarpable -> 8
    Queenspawnlarvatimer -> 11
    Ghostholdfire -> 12
    Ghostholdfireb -> 13
    Empdecloak -> 16
    Fungalgrowth -> 17
    Guardianshield -> 18
    Timewarpproduction -> 20
    Neuralparasite -> 22
    Stimpackmarauder -> 24
    Supplydrop -> 25
    Stimpack -> 27
    Psistorm -> 28
    Cloakfieldeffect -> 29
    Charging -> 30
    Slow -> 33
    Contaminated -> 36
    Blindingcloudstructure -> 38
    Oraclerevelation -> 49
    Viperconsumestructure -> 59
    Blindingcloud -> 83
    Medivacspeedboost -> 89
    Purify -> 97
    Oracleweapon -> 99
    Immortaloverload -> 102
    Lockon -> 116
    Seekermissile -> 120
    Temporalfield -> 121
    Voidrayswarmdamageboost -> 122
    Oraclestasistraptarget -> 129
    Parasiticbomb -> 132
    Parasiticbombunitku -> 133
    Parasiticbombsecondaryunitsearch -> 134
    Lurkerholdfireb -> 137
    Channelsnipecombat -> 145
    Tempestdisruptionblaststunbehavior -> 146
    Carrymineralfieldminerals -> 271
    Carryhighyieldmineralfieldminerals -> 272
    Carryharvestablevespenegeysergas -> 273
    Carryharvestablevespenegeysergasprotoss -> 274
    Carryharvestablevespenegeysergaszerg -> 275
    Chronoboosted -> 281

  --toEnum :: Int -> BuffId
  toEnum x = case x of
    0 -> Invalid
    5 -> Gravitonbeam
    6 -> Ghostcloak
    7 -> Bansheecloak
    8 -> Poweruserwarpable
    11 -> Queenspawnlarvatimer
    12 -> Ghostholdfire
    13 -> Ghostholdfireb
    16 -> Empdecloak
    17 -> Fungalgrowth
    18 -> Guardianshield
    20 -> Timewarpproduction
    22 -> Neuralparasite
    24 -> Stimpackmarauder
    25 -> Supplydrop
    27 -> Stimpack
    28 -> Psistorm
    29 -> Cloakfieldeffect
    30 -> Charging
    33 -> Slow
    36 -> Contaminated
    38 -> Blindingcloudstructure
    49 -> Oraclerevelation
    59 -> Viperconsumestructure
    83 -> Blindingcloud
    89 -> Medivacspeedboost
    97 -> Purify
    99 -> Oracleweapon
    102 -> Immortaloverload
    116 -> Lockon
    120 -> Seekermissile
    121 -> Temporalfield
    122 -> Voidrayswarmdamageboost
    129 -> Oraclestasistraptarget
    132 -> Parasiticbomb
    133 -> Parasiticbombunitku
    134 -> Parasiticbombsecondaryunitsearch
    137 -> Lurkerholdfireb
    145 -> Channelsnipecombat
    146 -> Tempestdisruptionblaststunbehavior
    271 -> Carrymineralfieldminerals
    272 -> Carryhighyieldmineralfieldminerals
    273 -> Carryharvestablevespenegeysergas
    274 -> Carryharvestablevespenegeysergasprotoss
    275 -> Carryharvestablevespenegeysergaszerg
    281 -> Chronoboosted
    _ -> Invalid