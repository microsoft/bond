-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Enum_java
    ( enum_java
    ) where

import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Java.Util as Java

-- Template for enum -> Java enum-like class.
enum_java :: MappingContext -> Declaration -> Text
enum_java java declaration = [lt|
package #{javaPackage};

#{typeDefinition declaration}
|]
  where
    javaPackage = sepBy "." toText $ getNamespace java

    typeDefinition Enum {..} = [lt|
#{Java.generatedClassAnnotations}
public final class #{declName} implements org.bondlib.BondEnum<#{declName}> {

    public static final class Values {
        private Values() {}

        #{newlineSep 2 constantIntValueDecl enumConstantsWithInt}
    }

    private static final class EnumBondTypeImpl extends org.bondlib.EnumBondType<#{declName}> {

        @Override
        public java.lang.Class<#{declName}> getValueClass() { return #{declName}.class; }

        @Override
        public final #{declName} getEnumValue(int value) { return get(value); }
    }

    public static final org.bondlib.EnumBondType<#{declName}> BOND_TYPE = new EnumBondTypeImpl();

    #{newlineSep 1 constantObjectDecl enumConstantsWithInt}

    public final int value;

    private final java.lang.String label;

    private #{declName}(int value, java.lang.String label) { this.value = value; this.label = label; }

    @Override
    public final int getValue() { return this.value; }

    @Override
    public final java.lang.String getLabel() { return this.label; }

    @Override
    public final org.bondlib.EnumBondType<#{declName}> getBondType() { return BOND_TYPE; }

    @Override
    public final int compareTo(#{declName} o) { return this.value < o.value ? -1 : (this.value > o.value ? 1 : 0); }

    @Override
    public final boolean equals(java.lang.Object other) { return (other instanceof #{declName}) && (this.value == ((#{declName}) other).value); }

    @Override
    public final int hashCode() { return this.value; }

    @Override
    public final java.lang.String toString() { return this.label != null ? this.label : ("#{declName}(" + java.lang.String.valueOf(this.value) + ")"); }

    public static #{declName} get(int value) {
        switch (value) {
            #{newlineSep 3 switchCaseConstantMapping enumConstantsWithIntDistinct}
            default: return new #{declName}(value, null);
        }
    }

    public static #{declName} valueOf(java.lang.String str) {
        if (str == null) {
            throw new java.lang.IllegalArgumentException("Argument 'str' must not be null.");
        #{newlineSepEnd 2 parseCaseConstantMapping enumConstants}} else {
            throw new java.lang.IllegalArgumentException("Invalid '#{declName}' enum value: '" + str + "'.");
        }
    }
}|]
      where
        -- constant object
        constantObjectDecl c@Constant {..} =
            [lt|public static final #{declName} #{constantName} = #{enumObjectAssigmentValue c};|]

        -- constant int
        constantIntValueDecl Constant {..} = let value x = [lt|#{x}|] in
            [lt|public static final int #{constantName} = #{optional value constantValue};|]

        -- switch cases that map int to object
        switchCaseConstantMapping Constant {..} =
            [lt|case Values.#{constantName}: return #{constantName};|]

        -- parse cases that map string to object
        parseCaseConstantMapping Constant {..} =
            [lt|} else if (str.equals("#{constantName}")) {
            return #{constantName};|]

        -- Process constants to make sure every constant value is set (either explicit or auto-generated).
        -- TODO: auto-generation of constant values should be handled earlier, once for all languages.
        enumConstantsWithInt = fixEnumWithInt 0 enumConstants []

        fixEnumWithInt :: Int -> [Constant] -> [Constant] -> [Constant]
        fixEnumWithInt _ [] result = reverse result
        fixEnumWithInt nextInt (h:r) result = case constantValue h of
          Just n -> let fixedN = (fromInteger (Java.twosComplement 32 (toInteger n))) in
            fixEnumWithInt (fixedN + 1) r ((Constant (constantName h) (Just fixedN)):result)
          Nothing -> fixEnumWithInt (nextInt + 1) r ((Constant (constantName h) (Just nextInt)):result)

        -- Filter a list of constants, leaving a list of constants with distinct values.
        -- If several constants in the input list share a value, the first one that appears will be the one that appears in the output list.
        enumConstantsWithIntDistinct = findEnumConstantsDistinct enumConstantsWithInt []

        findEnumConstantsDistinct :: [Constant] -> [Maybe Int] -> [Constant]
        findEnumConstantsDistinct [] _ = []
        findEnumConstantsDistinct (h:r) keys = if elem (constantValue h) keys
            then findEnumConstantsDistinct r keys
            else h : findEnumConstantsDistinct r ((constantValue h):keys)

        -- The RHS value to assign to an enum object. If an enum element is the first instance with a particular value,
        -- this function will return an instantiation. If it isn't, this function will return a reference to the first enum object with the same value.
        enumObjectAssigmentValue :: Constant -> Text
        enumObjectAssigmentValue enumConstant =
            let firstDeclaredEnumConstant = head (filter (\c -> (constantValue c) == (constantValue enumConstant)) enumConstantsWithIntDistinct) in
                if firstDeclaredEnumConstant == enumConstant
                    then [lt|new #{declName}(Values.#{constantName enumConstant}, "#{constantName enumConstant}")|]
                    else [lt|#{constantName firstDeclaredEnumConstant}|]

    typeDefinition _ = mempty
