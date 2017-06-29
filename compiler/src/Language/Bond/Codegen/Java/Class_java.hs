-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Class_java
    ( class_java
    , JavaFieldMapping(..)
    ) where

import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Java.Util


-- field -> public field
data JavaFieldMapping = JavaPublicFields deriving Eq


-- returns the name of the struct field descriptor type (a protected nested class within StructBondType)
structFieldDescriptorTypeName :: MappingContext -> Type -> Text
structFieldDescriptorTypeName java type_ = typeName type_
    where
        typeName (BT_Maybe BT_Int8) = [lt|com.microsoft.bond.StructBondType.SomethingInt8StructField|]
        typeName BT_Int8 = [lt|com.microsoft.bond.StructBondType.Int8StructField|]
        typeName (BT_Maybe BT_Int16) = [lt|com.microsoft.bond.StructBondType.SomethingInt16StructField|]
        typeName BT_Int16 = [lt|com.microsoft.bond.StructBondType.Int16StructField|]
        typeName (BT_Maybe BT_Int32) = [lt|com.microsoft.bond.StructBondType.SomethingInt32StructField|]
        typeName BT_Int32 = [lt|com.microsoft.bond.StructBondType.Int32StructField|]
        typeName (BT_Maybe BT_Int64) = [lt|com.microsoft.bond.StructBondType.SomethingInt64StructField|]
        typeName BT_Int64 = [lt|com.microsoft.bond.StructBondType.Int64StructField|]
        typeName (BT_Maybe BT_UInt8) = [lt|com.microsoft.bond.StructBondType.SomethingUInt8StructField|]
        typeName BT_UInt8 = [lt|com.microsoft.bond.StructBondType.UInt8StructField|]
        typeName (BT_Maybe BT_UInt16) = [lt|com.microsoft.bond.StructBondType.SomethingUInt16StructField|]
        typeName BT_UInt16 = [lt|com.microsoft.bond.StructBondType.UInt16StructField|]
        typeName (BT_Maybe BT_UInt32) = [lt|com.microsoft.bond.StructBondType.SomethingUInt32StructField|]
        typeName BT_UInt32 = [lt|com.microsoft.bond.StructBondType.UInt32StructField|]
        typeName (BT_Maybe BT_UInt64) = [lt|com.microsoft.bond.StructBondType.SomethingUInt64StructField|]
        typeName BT_UInt64 = [lt|com.microsoft.bond.StructBondType.UInt64StructField|]
        typeName (BT_Maybe BT_Float) = [lt|com.microsoft.bond.StructBondType.SomethingFloatStructField|]
        typeName BT_Float = [lt|com.microsoft.bond.StructBondType.FloatStructField|]
        typeName (BT_Maybe BT_Double) = [lt|com.microsoft.bond.StructBondType.SomethingDoubleStructField|]
        typeName BT_Double = [lt|com.microsoft.bond.StructBondType.DoubleStructField|]
        typeName (BT_Maybe BT_Bool) = [lt|com.microsoft.bond.StructBondType.SomethingBoolStructField|]
        typeName BT_Bool = [lt|com.microsoft.bond.StructBondType.BoolStructField|]
        typeName (BT_Maybe BT_String) = [lt|com.microsoft.bond.StructBondType.SomethingStringStructField|]
        typeName BT_String = [lt|com.microsoft.bond.StructBondType.StringStructField|]
        typeName (BT_Maybe BT_WString) = [lt|com.microsoft.bond.StructBondType.SomethingWStringStructField|]
        typeName BT_WString = [lt|com.microsoft.bond.StructBondType.WStringStructField|]
        typeName (BT_Maybe (BT_UserDefined e@Enum {..} _)) = [lt|com.microsoft.bond.StructBondType.SomethingWStringStructField<#{qualifiedDeclaredTypeName java e}>|]
        typeName(BT_UserDefined e@Enum {..} _) = [lt|com.microsoft.bond.StructBondType.EnumStructField<#{qualifiedDeclaredTypeName java e}>|]
        typeName (BT_Maybe t) = [lt|com.microsoft.bond.StructBondType.SomethingObjectStructField<#{(getTypeName java) t}>|]
        typeName t = [lt|com.microsoft.bond.StructBondType.ObjectStructField<#{(getTypeName java) t}>|]


-- returns whether a struct field descriptor type (a protected nested class within StructBondType) is generic and hence needs an explicit type parameter
isGenericStructFieldDescriptor :: Type -> Bool
isGenericStructFieldDescriptor (BT_Maybe t) = not (isPrimitiveNonEnumBondType t)
isGenericStructFieldDescriptor t = not (isPrimitiveNonEnumBondType t)


-- returns an type descriptor expression for a given struct (identified by a name and generic parameters), used in intiialization of struct field descriptors
structFieldDescriptorInitStructExpr :: MappingContext -> String -> [Type] -> Text
structFieldDescriptorInitStructExpr java qualifiedTypeName params = javaStructFieldDescriptorInitStructExpr
    where
        javaStructFieldDescriptorInitStructExpr = [lt|#{typeCastExpr}getStructType(#{qualifiedTypeName}.class#{paramExprList params})|]
            where
                typeCastExpr = [lt|(com.microsoft.bond.StructBondType<#{qualifiedTypeName}>)(com.microsoft.bond.StructBondType<?>)|]
                paramExprList :: [Type] -> Text
                paramExprList [] = mempty
                paramExprList (x:xs) = [lt|, #{structFieldDescriptorInitTypeExpr java x}#{paramExprList xs}|]


-- returns an type descriptor expression for a given field type, used in intiialization of struct field descriptors
structFieldDescriptorInitTypeExpr :: MappingContext -> Type -> Text
structFieldDescriptorInitTypeExpr java type_ = javaStructFieldDescriptorInitTypeExpr type_
    where
        javaStructFieldDescriptorInitTypeExpr (BT_Maybe t) = javaStructFieldDescriptorInitTypeExpr t
        javaStructFieldDescriptorInitTypeExpr BT_Int8 = [lt|com.microsoft.bond.BondTypes.INT8|]
        javaStructFieldDescriptorInitTypeExpr BT_Int16 = [lt|com.microsoft.bond.BondTypes.INT16|]
        javaStructFieldDescriptorInitTypeExpr BT_Int32 = [lt|com.microsoft.bond.BondTypes.INT32|]
        javaStructFieldDescriptorInitTypeExpr BT_Int64 = [lt|com.microsoft.bond.BondTypes.INT64|]
        javaStructFieldDescriptorInitTypeExpr BT_UInt8 = [lt|com.microsoft.bond.BondTypes.UINT8|]
        javaStructFieldDescriptorInitTypeExpr BT_UInt16 = [lt|com.microsoft.bond.BondTypes.UINT16|]
        javaStructFieldDescriptorInitTypeExpr BT_UInt32 = [lt|com.microsoft.bond.BondTypes.UINT32|]
        javaStructFieldDescriptorInitTypeExpr BT_UInt64 = [lt|com.microsoft.bond.BondTypes.UINT64|]
        javaStructFieldDescriptorInitTypeExpr BT_Float = [lt|com.microsoft.bond.BondTypes.FLOAT|]
        javaStructFieldDescriptorInitTypeExpr BT_Double = [lt|com.microsoft.bond.BondTypes.DOUBLE|]
        javaStructFieldDescriptorInitTypeExpr BT_Bool = [lt|com.microsoft.bond.BondTypes.BOOL|]
        javaStructFieldDescriptorInitTypeExpr BT_String = [lt|com.microsoft.bond.BondTypes.STRING|]
        javaStructFieldDescriptorInitTypeExpr BT_WString = [lt|com.microsoft.bond.BondTypes.WSTRING|]
        javaStructFieldDescriptorInitTypeExpr BT_Blob = [lt|com.microsoft.bond.BondTypes.BLOB|]
        javaStructFieldDescriptorInitTypeExpr (BT_Bonded t) = [lt|bondedOf(#{javaStructFieldDescriptorInitTypeExpr t})|]
        javaStructFieldDescriptorInitTypeExpr (BT_Nullable t) = [lt|nullableOf(#{javaStructFieldDescriptorInitTypeExpr t})|]
        javaStructFieldDescriptorInitTypeExpr (BT_Vector t) = [lt|vectorOf(#{javaStructFieldDescriptorInitTypeExpr t})|]
        javaStructFieldDescriptorInitTypeExpr (BT_List t) = [lt|listOf(#{javaStructFieldDescriptorInitTypeExpr t})|]
        javaStructFieldDescriptorInitTypeExpr (BT_Set t) = [lt|setOf(#{javaStructFieldDescriptorInitTypeExpr t})|]
        javaStructFieldDescriptorInitTypeExpr (BT_Map k v) = [lt|mapOf(#{javaStructFieldDescriptorInitTypeExpr k}, #{javaStructFieldDescriptorInitTypeExpr v})|]
        javaStructFieldDescriptorInitTypeExpr (BT_TypeParam param) = [lt|#{paramName param}|]
        javaStructFieldDescriptorInitTypeExpr (BT_UserDefined e@Enum {..} _) = [lt|#{qualifiedDeclaredTypeName java e}.BOND_TYPE|]
        javaStructFieldDescriptorInitTypeExpr (BT_UserDefined s@Struct {..} params) = [lt|#{structFieldDescriptorInitStructExpr java (qualifiedDeclaredTypeName java s) params}|]
        javaStructFieldDescriptorInitTypeExpr (BT_UserDefined s@Forward {..} params) = [lt|#{structFieldDescriptorInitStructExpr java (qualifiedDeclaredTypeName java s) params}|]
        javaStructFieldDescriptorInitTypeExpr t = error $ "invalid declaration type for javaFieldBondTypeExpression: " ++ (show t)


-- returns an type descriptor expression for a given struct base
structBaseDescriptorInitStructExpr :: MappingContext -> (Maybe Type) -> Text
structBaseDescriptorInitStructExpr _ Nothing = [lt|null|]
structBaseDescriptorInitStructExpr java (Just t) = structFieldDescriptorInitTypeExpr java t


-- returns an expression for the default value of a field that has an explicit default, used in intiialization of struct field descriptors
fieldDefaultValueInitParamExpr :: MappingContext -> Type -> Maybe Default -> Text
fieldDefaultValueInitParamExpr java type_ default_ = javaFieldDefaultValueInitParamExpr type_ default_
    where
        javaFieldDefaultValueInitParamExpr _ (Just (DefaultBool val)) = if val
            then [lt|true|]
            else [lt|false|]
        javaFieldDefaultValueInitParamExpr BT_Int8 (Just (DefaultInteger val)) = [lt|(byte)#{val}|]
        javaFieldDefaultValueInitParamExpr BT_Int16 (Just (DefaultInteger val)) = [lt|(short)#{val}|]
        javaFieldDefaultValueInitParamExpr BT_Int32 (Just (DefaultInteger val)) = [lt|#{val}|]
        javaFieldDefaultValueInitParamExpr BT_Int64 (Just (DefaultInteger val)) = [lt|#{val}L|]
        javaFieldDefaultValueInitParamExpr BT_UInt8 (Just (DefaultInteger val)) = [lt|(byte)#{val}|]
        javaFieldDefaultValueInitParamExpr BT_UInt16 (Just (DefaultInteger val)) = [lt|(short)#{val}|]
        javaFieldDefaultValueInitParamExpr BT_UInt32 (Just (DefaultInteger val)) = [lt|#{val}|]
        javaFieldDefaultValueInitParamExpr BT_UInt64 (Just (DefaultInteger val)) = [lt|#{val}L|]
        javaFieldDefaultValueInitParamExpr BT_Float (Just (DefaultFloat val)) = [lt|#{val}F|]
        javaFieldDefaultValueInitParamExpr BT_Double (Just (DefaultFloat val)) = [lt|#{val}D|]
        javaFieldDefaultValueInitParamExpr BT_String (Just (DefaultString val)) = [lt|"#{val}"|]
        javaFieldDefaultValueInitParamExpr BT_WString (Just (DefaultString val)) = [lt|"#{val}"|]
        javaFieldDefaultValueInitParamExpr (BT_UserDefined e@Enum {..} _) (Just (DefaultEnum val)) = [lt|#{qualifiedDeclaredTypeName java e}.#{val}|]
        javaFieldDefaultValueInitParamExpr t _ = error $ "invalid declaration type for fieldDefaultValueInitParamExpr: " ++ (show t)


-- builds text for GenericBondTypeBuilder abstract class
genericBondTypeBuilderDeclMakeText :: String -> [TypeParam] -> Text
genericBondTypeBuilderDeclMakeText declName declParams = [lt|
    public static abstract class GenericBondTypeBuilder {

        private GenericBondTypeBuilder() {
        }

        public abstract <#{paramList}> com.microsoft.bond.StructBondType<#{declName}<#{paramList}>> makeGenericType(#{sepBy ", " methodArg declParams});
    }
|]
    where
        paramList = sepBy ", " paramName declParams
        methodArg TypeParam {..} = [lt|com.microsoft.bond.BondType<#{paramName}> #{paramName}|]


-- builds text for implementation of the GenericBondTypeBuilder.makeGenericType method
makeGenericTypeMethodImplMakeText :: String -> [TypeParam] -> Text
makeGenericTypeMethodImplMakeText declName declParams = [lt|
            final <#{paramList}> com.microsoft.bond.StructBondType<#{declName}<#{paramList}>> makeGenericType(#{sepBy ", " methodArg declParams}) {
                #{newlineSep 3 checkArgument declParams}
                return (__StructBondTypeImpl<#{paramList}>) (com.microsoft.bond.StructBondType)this.getInitializedFromCache(#{paramList});
            }
|]
    where
        paramList = sepBy ", " paramName declParams
        methodArg TypeParam {..} = [lt|com.microsoft.bond.BondType<#{paramName}> #{paramName}|]
        checkArgument TypeParam {..} = [lt|com.microsoft.bond.helpers.ArgumentHelper.ensureNotNull(#{paramName}, "#{paramName}");|]


-- builds text for anonymous implementation of the GenericBondTypeBuilder abstract class and assignment to the BOND_TYPE variable
bondTypeStaticVariableDeclAsGenericBondTypeBuilder :: String -> [TypeParam] -> Text
bondTypeStaticVariableDeclAsGenericBondTypeBuilder declName declParams = [lt|public static final GenericBondTypeBuilder BOND_TYPE = new GenericBondTypeBuilder() {

        final __StructBondTypeImpl.__StructBondTypeBuilderImpl builder = new __StructBondTypeImpl.__StructBondTypeBuilderImpl();

        @Override
        public final <#{paramList}> StructBondType<#{declName}<#{paramList}>> makeGenericType(#{sepBy ", " methodArg declParams}) {
            return this.builder.makeGenericType(#{paramList});
        }
    };|]
    where
        paramList = sepBy ", " paramName declParams
        methodArg TypeParam {..} = [lt|com.microsoft.bond.BondType<#{paramName}> #{paramName}|]


-- builds text for public constructor of non-generic Bond struct class
publicConstructorDeclForNonGenericStruct :: MappingContext -> String -> (Maybe Type) -> Text
publicConstructorDeclForNonGenericStruct java declName maybeBase = [lt|
    public #{declName}() {
        super(#{superConstructorArgs maybeBase});
        ((__StructBondTypeImpl)BOND_TYPE).initializeStructFields(this);
    };
|]
    where
        superConstructorArgs Nothing = mempty
        superConstructorArgs (Just t) = if isGenericBondStructType t
            then [lt|(com.microsoft.bond.StructBondType<#{getTypeName java t}>)BOND_TYPE.getBaseStructType()|]
            else mempty


-- builds text for public constructor of generic Bond struct class
publicConstructorDeclForGenericStruct :: MappingContext -> String -> [TypeParam] -> (Maybe Type) -> Text
publicConstructorDeclForGenericStruct java declName declParams maybeBase = [lt|
public #{declName}(com.microsoft.bond.StructBondType<#{declName}<#{paramList}>> genericType) {
        super(#{superConstructorArgs maybeBase});
        this.__genericType = (__StructBondTypeImpl<#{paramList}>)genericType;
        this.__genericType.initializeStructFields(this);
    };
|]
    where
        paramList = sepBy ", " paramName declParams
        superConstructorArgs Nothing = mempty
        superConstructorArgs (Just t) = if isGenericBondStructType t
            then [lt|((com.microsoft.bond.StructBondType<#{getTypeName java t}>)com.microsoft.bond.helpers.ArgumentHelper.ensureNotNull(genericType, "genericType").getBaseStructType()|]
            else mempty


-- Template for struct -> Java class.
class_java :: MappingContext -> [Import] -> Declaration -> Text
class_java java _ declaration = [lt|
package #{javaPackage};
#{typeDefinition declaration}
|]
    where
        javaType = getTypeName java
        javaPackage = sepBy "." toText $ getNamespace java

        -- struct -> Java class
        typeDefinition Struct {..} = [lt|
#{generatedClassAnnotations}
public class #{declName}#{params}#{maybe interface baseClass structBase} {
    #{genericBondTypeBuilderDecl}

    private static final class __StructBondTypeImpl#{params} extends com.microsoft.bond.StructBondType<#{declName}#{params}> {

        static final class __StructBondTypeBuilderImpl extends com.microsoft.bond.StructBondType.StructBondTypeBuilder<#{declName}> {
            #{makeGenericTypeMethodImpl}

            @Override
            public final int getGenericTypeParameterCount() {
                return #{length declParams};
            }

            @Override
            protected final StructBondType<#{declName}> buildNewInstance(BondType<?>[] genericTypeArguments) {
                #{buildNewInstanceMethodBody}
            }

            static void register() {
                registerStructType(#{declName}.class, new __StructBondTypeBuilderImpl());
            }
        }

        #{doubleLineSep 2 fieldDescriptorFieldDecl structFields}

        #{structBondTypeImplConstructorDecl} {
            #{structBondTypeImplConstructorBody}
        }

        @Override
        protected final void initialize() {
            #{newlineSep 3 genericTypeArgumentVariableDecl declParamsWithIndex}
            #{newlineSep 3 fieldDescriptorFieldInit structFields}
            super.initializeBaseAndFields(#{initializeBaseAndFieldsBaseParam}#{initializeBaseAndFieldsFieldsParam});
        }

        @Override
        public final java.lang.Class<#{declName}#{params}> getValueClass() {
            return (java.lang.Class<#{declName}#{params}>) (java.lang.Class<?>) #{declName}.class;
        }

        @Override
        public final #{declName}#{params} newInstance() {
            #{newInstanceMethodBody}
        }

        @Override
        protected final void serializeStructFields(com.microsoft.bond.BondType.SerializationContext context, #{declName}#{params} value) throws java.io.IOException {
            #{newlineSep 3 serializeStructField structFields}
        }

        @Override
        protected final void deserializeStructFields(com.microsoft.bond.BondType.TaggedDeserializationContext context, #{declName}#{params} value) throws java.io.IOException {
            #{newlineSep 3 deserializeStructFieldDeclareBooleans structFields}
            while (readField(context)) {
                switch (context.readFieldResult.id) {
                    #{newlineSep 5 deserializeStructFieldProcessFieldCases structFields}
                }
            }
            #{newlineSep 3 deserializeStructFieldVerifyFields structFields}
        }

        @Override
        public final void initializeStructFields(#{declName}#{params} value) {
            #{newlineSep 3 initializeStructField structFields}
        }
    }

    #{bondTypeStaticVariableDecl}

    public static void initializeBondType() {
        __StructBondTypeImpl.__StructBondTypeBuilderImpl.register();
    }

    static {
        initializeBondType();
    }
    #{bondTypeDescriptorInstanceVariableDecl}

    #{doubleLineSep 1 publicFieldDecl structFields}

    #{publicConstructorDecl}

    @Override
    public com.microsoft.bond.StructBondType<? extends com.microsoft.bond.BondSerializable> getBondType() {
        return #{getBondTypeReturnValue};
    }
}|]
            where
                interface = [lt| implements com.microsoft.bond.BondSerializable|]
                params = angles $ sepBy ", " paramName declParams
                declParamsWithIndex = zip [0 :: Int ..] declParams
                baseClass x = [lt| extends #{javaType x}|]
                publicFieldDecl Field {..} = [lt|public #{javaType fieldType} #{fieldName};|]
                fieldDescriptorFieldDecl Field {..} = [lt|private #{structFieldDescriptorTypeName java fieldType} #{fieldName};|]
                fieldDescriptorFieldInit Field {..} = [lt|this.#{fieldName} = new #{structFieldDescriptorTypeName java fieldType}(#{initMethodParams});|]
                        where
                            initMethodParams = [lt|this#{fieldTypeParam}, #{fieldOrdinal}, "#{fieldName}", #{modifierConstantName fieldModifier}#{fieldDefaultValueParam}|]
                                where
                                    fieldTypeParam = if isGenericStructFieldDescriptor fieldType
                                        then [lt|, #{structFieldDescriptorInitTypeExpr java fieldType}|]
                                        else mempty
                                    fieldDefaultValueParam = if not (isPrimitiveBondType fieldType) || (fieldDefault == Nothing)
                                        then mempty
                                        else [lt|, #{fieldDefaultValueInitParamExpr java fieldType fieldDefault}|]
                initializeBaseAndFieldsBaseParam = structBaseDescriptorInitStructExpr java structBase
                initializeBaseAndFieldsFieldsParam = if null structFields
                    then mempty
                    else [lt|, #{sepBy ", " structFieldReference structFields}|]
                    where
                        structFieldReference Field {..} = [lt|this.#{fieldName}|]
                buildNewInstanceMethodBody = if null declParams
                    then [lt|return new __StructBondTypeImpl();|]
                    else [lt|return new __StructBondTypeImpl(new com.microsoft.bond.GenericTypeSpecialization(genericTypeArguments));|]
                structBondTypeImplConstructorDecl = if null declParams
                    then [lt|__StructBondTypeImpl()|]
                    else [lt|__StructBondTypeImpl(GenericTypeSpecialization genericTypeSpecialization)|]
                structBondTypeImplConstructorBody = if null declParams
                    then [lt|super(null);|]
                    else [lt|super(genericTypeSpecialization);|]
                newInstanceMethodBody = if null declParams
                    then [lt|return new #{declName}();|]
                    else [lt|return new #{declName}#{params}(this);|]
                genericTypeArgumentVariableDecl (index, TypeParam {..}) = [lt|com.microsoft.bond.BondType<#{paramName}> #{paramName} = this.getGenericSpecialization().getGenericTypeArgument(#{index});|]
                serializeStructField Field {..} = [lt|this.#{fieldName}.serialize(context, value.#{fieldName});|]
                deserializeStructFieldDeclareBooleans Field {..} = [lt|boolean __has_#{fieldName} = false;|]
                deserializeStructFieldProcessFieldCases Field {..} = [lt|#{casePart}#{newLine 6}#{deserializePart}#{newLine 7}#{setBooleanPart}#{newLine 7}break;|]
                    where
                        casePart = [lt|case #{fieldOrdinal}:|]
                        deserializePart = [lt|value.#{fieldName} = this.#{fieldName}.deserialize(context, __has_#{fieldName});|]
                        setBooleanPart = [lt|__has_#{fieldName} = true;|]
                deserializeStructFieldVerifyFields Field {..} = [lt|this.#{fieldName}.verifyDeserialized(__has_#{fieldName});|]
                initializeStructField Field {..} = [lt|value.#{fieldName} = this.#{fieldName}.initialize();|]
                genericBondTypeBuilderDecl = if null declParams
                    then mempty
                    else genericBondTypeBuilderDeclMakeText declName declParams
                makeGenericTypeMethodImpl = if null declParams
                    then mempty
                    else makeGenericTypeMethodImplMakeText declName declParams
                bondTypeStaticVariableDecl = if null declParams
                    then [lt|public static final com.microsoft.bond.StructBondType<#{declName}> BOND_TYPE = new __StructBondTypeImpl.__StructBondTypeBuilderImpl().getInitializedFromCache();|]
                    else bondTypeStaticVariableDeclAsGenericBondTypeBuilder declName declParams
                bondTypeDescriptorInstanceVariableDecl = if null declParams
                    then mempty
                    else [lt|private final __StructBondTypeImpl#{params} __genericType;|]
                getBondTypeReturnValue = if null declParams
                    then [lt|BOND_TYPE|]
                    else [lt|this.__genericType|]
                publicConstructorDecl = if null declParams
                    then publicConstructorDeclForNonGenericStruct java declName structBase
                    else publicConstructorDeclForGenericStruct java declName declParams structBase

        typeDefinition _ = mempty
