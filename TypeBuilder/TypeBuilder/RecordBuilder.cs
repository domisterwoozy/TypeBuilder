using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeBuilder
{
    public class RecordBuilder
    {             
        public INamedTypeSymbol TypeSymbol { get; }
        public TypeDeclarationSyntax TypeDeclSyntax { get; }
        public IMethodSymbol ConstructorSymbol { get; }

        public IEnumerable<IPropertySymbol> PropertySymbols => NameSymbolMapping.Values;
        public IEnumerable<PropertyDeclarationSyntax> PropertySyntaxes => SymbolSyntaxMapping.Values;
        public IEnumerable<string> PropertyNames => NameSymbolMapping.Keys;

        /// <summary>
        /// Indexes the property symbol by the proeprties Name.
        /// </summary>
        public IImmutableDictionary<string, IPropertySymbol> NameSymbolMapping { get; }
        /// <summary>
        /// Retrieves the syntax from a property symbol.
        /// </summary>
        public IImmutableDictionary<IPropertySymbol, PropertyDeclarationSyntax> SymbolSyntaxMapping { get; }
        /// <summary>
        /// A mapping of a constructor parameter to a read only property that it initializes.
        /// </summary>
        public IImmutableDictionary<IParameterSymbol, IPropertySymbol> ParamaterSymbolMapping { get; }

        public PropertyDeclarationSyntax SyntaxFromName(string name) => SymbolSyntaxMapping[NameSymbolMapping[name]];

        private RecordBuilder(IMethodSymbol methodSymbol, TypeDeclarationSyntax typeDecl, IImmutableDictionary<IPropertySymbol, PropertyDeclarationSyntax> symbolSyntaxMap)
        {           
            ConstructorSymbol = methodSymbol;
            TypeDeclSyntax = typeDecl;
            TypeSymbol = methodSymbol.ContainingType;
            NameSymbolMapping = symbolSyntaxMap.Keys.ToImmutableDictionary(prop => prop.Name, prop => prop);
            SymbolSyntaxMapping = symbolSyntaxMap;
            ParamaterSymbolMapping = GetMapping();
        }

        public static async Task<RecordBuilder> CreateMap(TypeDeclarationSyntax typeDecl, IMethodSymbol constructorSymbol)
        {
            if (constructorSymbol == null) throw new ArgumentNullException(nameof(constructorSymbol));
            if (typeDecl == null) throw new ArgumentNullException(nameof(typeDecl));
            if (constructorSymbol.MethodKind != MethodKind.Constructor) throw new ArgumentException(nameof(constructorSymbol) + " must be an instance constructor");
            var readOnlyProps = constructorSymbol.ContainingType.GetReadOnlyProperties().ToArray();
            if (readOnlyProps.Length != constructorSymbol.Parameters.Length) throw new ArgumentException(nameof(constructorSymbol) + " constructor must initialize all read only properties");

            if (readOnlyProps.Select(prop => prop.Name).Any(name => !char.IsUpper(name[0]))) throw new ArgumentException("All readonly properties must start with an uppercase char");
            var syntaxRefs = readOnlyProps.Select(prop => prop.DeclaringSyntaxReferences);
            if (syntaxRefs.Any(refs => refs.Length != 1)) throw new ArgumentException("All readonly properties must be fully declared (no partial declarations)");

            var propSyntaxes = await Task.WhenAll(syntaxRefs.Select(refs => refs.Single().GetSyntaxAsync()));
            var symbolSyntaxMap =
                readOnlyProps.Zip(propSyntaxes.Cast<PropertyDeclarationSyntax>(), (sym, syntax) => new { Symbol = sym, Syntax = syntax })
                .ToImmutableDictionary(val => val.Symbol, val => val.Syntax);
            return new RecordBuilder(constructorSymbol, typeDecl, symbolSyntaxMap);
        }

        // a mapping occurs when a parameter has the same name (when first char is uppered) and type as a readonly property
        private ImmutableDictionary<IParameterSymbol, IPropertySymbol> GetMapping()
        {
            return
                ConstructorSymbol.Parameters
                .Where(param => NameSymbolMapping.ContainsKey(param.Name.ToFirstCharUpper()))
                .Where(param => param.Type.Equals(NameSymbolMapping[param.Name.ToFirstCharUpper()].Type))
                .ToImmutableDictionary(param => param, param => NameSymbolMapping[param.Name.ToFirstCharUpper()]);
        }

        public MethodDeclarationSyntax CreateWith(IPropertySymbol withProperty)
        {
            if (withProperty == null) throw new ArgumentNullException(nameof(withProperty));
            if (!PropertySymbols.Contains(withProperty)) throw new ArgumentException(nameof(withProperty) + " is not part of the record.");

            var typeIdentifierNode = SyntaxFactory.IdentifierName(TypeDeclSyntax.Identifier.ValueText);

            var propTypeNode = SymbolSyntaxMapping[withProperty].Type;
            var propNameToken = SymbolSyntaxMapping[withProperty].Identifier;

            var paramNameNode = SyntaxFactory.IdentifierName($"new{withProperty.Name}"); // node
            var paramNameToken = SyntaxFactory.Identifier($"new{withProperty.Name}"); // token

            var objCreationArgs =
                SyntaxFactory.ArgumentList()
                .AddArguments(ConstructorSymbol.Parameters.Select(param => SyntaxFactory.Argument(
                        param.Name.Equals(withProperty.Name, StringComparison.OrdinalIgnoreCase) ? paramNameNode : SyntaxFactory.IdentifierName(ParamaterSymbolMapping[param].Name))).ToArray());

            var methodParams =
                SyntaxFactory.ParameterList()
                .AddParameters(SyntaxFactory.Parameter(SyntaxFactory.List<AttributeListSyntax>(), SyntaxFactory.TokenList(), propTypeNode, paramNameToken, null));

            var withMethod =
                SyntaxFactory.MethodDeclaration(typeIdentifierNode, $"With{withProperty.Name}")
                .WithParameterList(methodParams)
                .WithModifiers(RoslynUtil.PublicModifiers)
                .WithExpressionBody(SyntaxFactory.ArrowExpressionClause(SyntaxFactory.ObjectCreationExpression(typeIdentifierNode, objCreationArgs, null)))
                .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken));        

            return withMethod;
        }

        public IEnumerable<SyntaxNode> CreateEqualityMethods()
        {
            yield return CreateEqualsOverride();
            yield return CreateHashCodeOverride();
            yield return CreateToStringOverride();
        }     

        public MethodDeclarationSyntax CreateEqualsOverride()
        {
            var returnFalseStatement = SyntaxFactory.ReturnStatement(RoslynUtil.FalseLiteral);
            var returnTrueStatement = SyntaxFactory.ReturnStatement(RoslynUtil.TrueLiteral);

            var paramNameToken = SyntaxFactory.Identifier("obj");
            var paramNameNode = SyntaxFactory.IdentifierName("obj");
            var typeIdentifierNode = SyntaxFactory.IdentifierName(TypeDeclSyntax.Identifier.ValueText);

            IfStatementSyntax castCheckStatement =
                SyntaxFactory.IfStatement(
                    SyntaxFactory.PrefixUnaryExpression(
                        SyntaxKind.LogicalNotExpression,
                        SyntaxFactory.ParenthesizedExpression(
                            SyntaxFactory.BinaryExpression(SyntaxKind.IsExpression, paramNameNode, typeIdentifierNode))),
                    returnFalseStatement);

            LocalDeclarationStatementSyntax strongTypeDecl =
                SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(
                            RoslynUtil.VarKeyword,
                            SyntaxFactory.SeparatedList(new[] {
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier("typedObj"),
                                    null,
                                    SyntaxFactory.EqualsValueClause(SyntaxFactory.CastExpression(typeIdentifierNode, paramNameNode)))})));

            IEnumerable<IfStatementSyntax> propChecks =
                PropertyNames.Select(name =>
                    SyntaxFactory.IfStatement(
                    SyntaxFactory.PrefixUnaryExpression(
                        SyntaxKind.LogicalNotExpression,
                        SyntaxFactory.InvocationExpression(
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.IdentifierName(name), // property to access
                                SyntaxFactory.IdentifierName("Equals")), // method to call
                            SyntaxFactory.ArgumentList(SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        SyntaxFactory.IdentifierName("typedObj"),
                                        SyntaxFactory.IdentifierName(name))))))),
                    returnFalseStatement));


            var methodParams =
                SyntaxFactory.ParameterList()
                .AddParameters(SyntaxFactory.Parameter(SyntaxFactory.List<AttributeListSyntax>(), SyntaxFactory.TokenList(), RoslynUtil.ObjectKeyword, paramNameToken, null));

            var equalsMethod =
                SyntaxFactory.MethodDeclaration(RoslynUtil.BoolKeyword, "Equals")
                .WithModifiers(RoslynUtil.PublicOverrideModifiers)
                .WithParameterList(methodParams)
                .WithBody(SyntaxFactory.Block(new StatementSyntax[] { castCheckStatement, strongTypeDecl }.Concat(propChecks).Concat(returnTrueStatement)));

            return equalsMethod;
        }

        public MethodDeclarationSyntax CreateHashCodeOverride()
        {
            var typeIdentifierNode = SyntaxFactory.IdentifierName(TypeDeclSyntax.Identifier.ValueText);
            var intTypeSyntax = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.IntKeyword));

            LocalDeclarationStatementSyntax hashCodeDecl =
                SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(
                            intTypeSyntax,
                            SyntaxFactory.SeparatedList(new[] {
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier("hashCode"),
                                    null,
                                    SyntaxFactory.EqualsValueClause(SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(17))))})));

            IEnumerable<ExpressionStatementSyntax> loopExpressions =
                PropertyNames.Select(name =>
                    SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                SyntaxFactory.IdentifierName("hashCode"),
                                SyntaxFactory.Token(SyntaxKind.EqualsToken),
                                SyntaxFactory.BinaryExpression(
                                    SyntaxKind.AddExpression,
                                    SyntaxFactory.BinaryExpression(
                                        SyntaxKind.MultiplyExpression,
                                        SyntaxFactory.IdentifierName("hashCode"),
                                        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(23))),
                                    SyntaxFactory.InvocationExpression(SyntaxFactory.MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        SyntaxFactory.IdentifierName(name),
                                        SyntaxFactory.IdentifierName("GetHashCode")))))));

            ReturnStatementSyntax returnStatement = SyntaxFactory.ReturnStatement(SyntaxFactory.IdentifierName("hashCode"));

            var hashCodeMethod =
                SyntaxFactory.MethodDeclaration(intTypeSyntax, "GetHashCode")
                .WithModifiers(RoslynUtil.PublicOverrideModifiers)
                .WithBody(SyntaxFactory.Block(hashCodeDecl.Concat<StatementSyntax>(loopExpressions).Concat(returnStatement)));                    

            return hashCodeMethod;          
        }

        public MethodDeclarationSyntax CreateToStringOverride()
        {
            var stringTypeSyntax = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.StringKeyword));

            var stringExpression =
                SyntaxFactory.InterpolatedStringExpression(
                    SyntaxFactory.Token(SyntaxKind.InterpolatedStringStartToken),
                    SyntaxFactory.List(PropertyNames.SelectMany(p => new InterpolatedStringContentSyntax[] {
                        RoslynUtil.InterpolatedTextLiteral($"{p}: '"),
                        SyntaxFactory.Interpolation(SyntaxFactory.IdentifierName(p)),
                        RoslynUtil.InterpolatedTextLiteral("';") })),
                    SyntaxFactory.Token(SyntaxKind.InterpolatedStringEndToken));

            ReturnStatementSyntax returnStatement = SyntaxFactory.ReturnStatement(stringExpression);

            var toStringMethod =
                SyntaxFactory.MethodDeclaration(stringTypeSyntax, "ToString")
                .WithModifiers(RoslynUtil.PublicOverrideModifiers)
                .WithBody(SyntaxFactory.Block(returnStatement));

            return toStringMethod;
        }

        public ClassDeclarationSyntax CreateBuilder()
        {
            var typeSyntax = SyntaxFactory.IdentifierName(TypeDeclSyntax.Identifier.ValueText);

            var objCreationArgs =
                SyntaxFactory.ArgumentList()
                .AddArguments(ConstructorSymbol.Parameters.Select(param => SyntaxFactory.Argument(SyntaxFactory.IdentifierName(ParamaterSymbolMapping[param].Name))).ToArray());

            var buildMethod =
                SyntaxFactory.MethodDeclaration(typeSyntax, "Build")
                .WithModifiers(RoslynUtil.PublicModifiers)
                .WithBody(SyntaxFactory.Block(SyntaxFactory.ReturnStatement(SyntaxFactory.ObjectCreationExpression(typeSyntax, objCreationArgs, null))));


            var builderTypeNode =
                SyntaxFactory.ClassDeclaration("Builder")
                .WithModifiers(RoslynUtil.PublicModifiers)
                .AddMembers(PropertySyntaxes.Select(prop => prop.WithAccessorList(RoslynUtil.GetSetAccessors)).ToArray())
                .AddMembers(buildMethod);

            return builderTypeNode;
        }        
    }
}
