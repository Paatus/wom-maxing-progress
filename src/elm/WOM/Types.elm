module WOM.Types exposing
    ( Activity(..)
    , ActivityObj
    , Boss(..)
    , BossObj
    , ComputedMetric(..)
    , DeltaObj(..)
    , DiffObj
    , EHPMethod
    , EHPObject
    , EHPRates
    , GainedData
    , Period(..)
    , Skill(..)
    , SkillObj
    , TopItems
    )

import Dict exposing (Dict)


type Period
    = Five_min
    | Day
    | Week
    | Month
    | Year


type Skill
    = Overall
    | Attack
    | Defence
    | Strength
    | Hitpoints
    | Ranged
    | Prayer
    | Magic
    | Cooking
    | Woodcutting
    | Fletching
    | Fishing
    | Firemaking
    | Crafting
    | Smithing
    | Mining
    | Herblore
    | Agility
    | Thieving
    | Slayer
    | Farming
    | Runecrafting
    | Hunter
    | Construction


type Activity
    = League_points
    | Bounty_hunter_hunter
    | Bounty_hunter_rogue
    | Clue_scrolls_all
    | Clue_scrolls_beginner
    | Clue_scrolls_easy
    | Clue_scrolls_medium
    | Clue_scrolls_hard
    | Clue_scrolls_elite
    | Clue_scrolls_master
    | Last_man_standing
    | Pvp_arena
    | Soul_wars_zeal
    | Guardians_of_the_rift


type Boss
    = Abyssal_sire
    | Alchemical_hydra
    | Barrows_chests
    | Bryophyta
    | Callisto
    | Cerberus
    | Chambers_of_xeric
    | Chambers_of_xeric_challenge_mode
    | Chaos_elemental
    | Chaos_fanatic
    | Commander_zilyana
    | Corporeal_beast
    | Crazy_archaeologist
    | Dagannoth_prime
    | Dagannoth_rex
    | Dagannoth_supreme
    | Deranged_archaeologist
    | General_graardor
    | Giant_mole
    | Grotesque_guardians
    | Hespori
    | Kalphite_queen
    | King_black_dragon
    | Kraken
    | Kreearra
    | Kril_tsutsaroth
    | Mimic
    | Nex
    | Nightmare
    | Phosanis_nightmare
    | Obor
    | Sarachnis
    | Scorpia
    | Skotizo
    | Tempoross
    | The_gauntlet
    | The_corrupted_gauntlet
    | Theatre_of_blood
    | Theatre_of_blood_hard_mode
    | Thermonuclear_smoke_devil
    | Tombs_of_amascut
    | Tombs_of_amascut_expert
    | Tzkal_zuk
    | Tztok_jad
    | Venenatis
    | Vetion
    | Vorkath
    | Wintertodt
    | Zalcano
    | Zulrah


type ComputedMetric
    = Ehp
    | Ehb


type alias DiffObj =
    { gained : Int
    , start : Int
    , end : Int
    }


type alias SkillObj =
    { name : String
    , experience : DiffObj
    , rank : DiffObj
    , level : DiffObj
    }


type alias ActivityObj =
    { name : String
    , score : DiffObj
    , rank : DiffObj
    }


type alias BossObj =
    { name : String
    , kills : DiffObj
    , rank : DiffObj
    }


type DeltaObj
    = BossDelta BossObj
    | ActivityDelta ActivityObj
    | SkillDelta SkillObj


type alias GainedData =
    { skill : Dict String SkillObj
    , activity : Dict String ActivityObj
    , boss : Dict String BossObj
    }


type alias EHPMethod =
    { methodName : String
    , rate : Int
    , fromExp : Int
    }


type alias EHPObject =
    { skill : String
    , methods : List EHPMethod
    }


type alias EHPRates =
    Dict String EHPObject


type alias TopItems =
    { gainedExperience : Maybe DeltaObj
    , topBoss : Maybe DeltaObj
    , topSkill : Maybe DeltaObj
    , topActivity : Maybe DeltaObj
    }
