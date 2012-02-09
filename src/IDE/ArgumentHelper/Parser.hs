module IDE.ArgumentHelper.Parser (
    parseArgumentsFromMethodDeclaration
    , ArgumentType (..)
) where

import Language.Haskell.Exts
import Data.String.Utils (replace)


data ArgumentType = ArgumentTypePlain String | ArgumentTypeTuple [ArgumentType]


-- | Return all arguments of given method declaration.
parseArgumentsFromMethodDeclaration :: String -> [ArgumentType]
parseArgumentsFromMethodDeclaration methodSig = case
    ((parseDeclWithMode defaultParseMode{extensions = [OverlappingInstances,
            UndecidableInstances,
            IncoherentInstances,
            RecursiveDo,
            ParallelListComp,
            MultiParamTypeClasses,
            NoMonomorphismRestriction,
            FunctionalDependencies,
            ExplicitForall,
            Rank2Types,
            RankNTypes,
            PolymorphicComponents,
            ExistentialQuantification,
            ScopedTypeVariables,
            ImplicitParams,
            FlexibleContexts,
            FlexibleInstances,
            EmptyDataDecls,
            CPP,
            KindSignatures,
            BangPatterns,
            TypeSynonymInstances,
            TemplateHaskell,
            ForeignFunctionInterface,
            Arrows,
            Generics,
            NoImplicitPrelude,
            NamedFieldPuns,
            PatternGuards,
            GeneralizedNewtypeDeriving,
            ExtensibleRecords,
            RestrictedTypeSynonyms,
            HereDocuments,
            MagicHash,
            TypeFamilies,
            StandaloneDeriving,
            UnicodeSyntax,
            PatternSignatures,
            UnliftedFFITypes,
            LiberalTypeSynonyms,
            TypeOperators,
            RecordWildCards,
            RecordPuns,
            DisambiguateRecordFields,
            OverloadedStrings,
            GADTs,
            MonoPatBinds,
            NoMonoPatBinds,
            RelaxedPolyRec,
            ExtendedDefaultRules,
            UnboxedTuples,
            DeriveDataTypeable,
            ConstrainedClassMethods,
            PackageImports,
            ImpredicativeTypes,
            NewQualifiedOperators,
            PostfixOperators,
            QuasiQuotes,
            TransformListComp,
            ViewPatterns,
            XmlSyntax,
            RegularPatterns,
            TupleSections]} $ removeAllAfterEqualSign $ filterDisallowed methodSig)
        -- TODO parsing fails if no <methodName> :: is contained (e.g. fold)
        >>= parseArgumentsFromDecl)
            of
                ParseOk [] -> []
                ParseOk fnTypes -> init fnTypes -- we do not want the return type
                ParseFailed _ _ -> [ArgumentTypePlain "parsing failed"]


parseArgumentsFromDecl :: Decl -> ParseResult [ArgumentType]
parseArgumentsFromDecl (TypeSig _ _ ty) = return $ map makeArgumentType $ parseTyFun ty
parseArgumentsFromDecl _ = return [] -- TODO implement correctly


parseTyFun :: Type -> [Type]
parseTyFun (TyFun t1 t2) = t1:(parseTyFun t2)
parseTyFun (TyForall _ _ t) = parseTyFun t
parseTyFun (TyInfix t1 _ t2) = parseTyFun t2
parseTyFun t = [t]


makeArgumentType :: Type -> ArgumentType
makeArgumentType (TyTuple _ types) = ArgumentTypeTuple $ map makeArgumentType types
makeArgumentType t                 = ArgumentTypePlain $ prettyPrint t


filterDisallowed :: String -> String
filterDisallowed = replace "<document comment>" ""

-- WORKAROUND for buggy methodDecl containing actual code
removeAllAfterEqualSign :: String -> String
removeAllAfterEqualSign (x:xs)
    | x == '='  = ""
    | otherwise = (x:(removeAllAfterEqualSign xs))
removeAllAfterEqualSign "" = ""


--parseTyFun (TyTuple _ typeList) =
--parseTyFun (TyList t) =
--TyVar Name
--TyParen Type
--TyCon QName
--TyKind Type Kind

