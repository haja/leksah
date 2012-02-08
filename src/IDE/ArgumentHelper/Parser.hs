module IDE.ArgumentHelper.Parser (
    parseArgumentsFromMethodDeclaration
) where

import Language.Haskell.Exts
import Data.String.Utils (replace)


-- | Return all arguments of given method declaration.
parseArgumentsFromMethodDeclaration :: String -> [String]
parseArgumentsFromMethodDeclaration methodName = case
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
            TupleSections]} $ filterDisallowed methodName)
        >>= parseArgumentsFromDecl)
            of
                ParseOk fnTypes -> init fnTypes -- we do not want the return type
                ParseFailed _ _ -> ["parsing failed"]


parseArgumentsFromDecl :: Decl -> ParseResult [String]
parseArgumentsFromDecl (TypeSig _ _ ty) = return $ map prettyPrint $ parseTyFun ty

parseTyFun :: Type -> [Type]
parseTyFun (TyFun t1 t2) = t1:(parseTyFun t2)
parseTyFun (TyForall _ _ t) = parseTyFun t
parseTyFun (TyInfix t1 _ t2) = parseTyFun t2
parseTyFun t = [t]


filterDisallowed :: String -> String
filterDisallowed = replace "<document comment>" ""



--parseTyFun (TyTuple _ typeList) =
--parseTyFun (TyList t) =
--TyVar Name
--TyParen Type
--TyCon QName
--TyKind Type Kind

