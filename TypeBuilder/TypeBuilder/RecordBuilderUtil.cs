using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeBuilder
{
    public static class RoslynUtil
    {
        // predefined type keywords (string, int, object, bool, etc)
        public static readonly PredefinedTypeSyntax ObjectKeyword = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.ObjectKeyword));
        public static readonly PredefinedTypeSyntax BoolKeyword = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.BoolKeyword));

        public static readonly IdentifierNameSyntax VarKeyword = SyntaxFactory.IdentifierName("var");

        public static AccessorListSyntax GetSetAccessors =
            SyntaxFactory.AccessorList(SyntaxFactory.List(new[]
            {
                SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration).WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken)),
                SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration).WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
            }));

        public static readonly LiteralExpressionSyntax FalseLiteral = SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression);
        public static readonly LiteralExpressionSyntax TrueLiteral = SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression);

        // modifier lists
        public static readonly SyntaxTokenList PublicModifiers = SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
        public static readonly SyntaxTokenList PublicOverrideModifiers = SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword), SyntaxFactory.Token(SyntaxKind.OverrideKeyword));

        public static InterpolatedStringTextSyntax InterpolatedTextLiteral(string txt)
        {
            return
                SyntaxFactory.InterpolatedStringText(
                    SyntaxFactory.Token(
                        SyntaxFactory.TriviaList(),
                        SyntaxKind.InterpolatedStringTextToken,
                        txt, txt,                          
                        SyntaxFactory.TriviaList()));
        }

        public static IEnumerable<IPropertySymbol> GetReadOnlyProperties(this INamedTypeSymbol typeSymbol)
        {
            if (typeSymbol == null) throw new ArgumentNullException(nameof(typeSymbol));
            return
                typeSymbol.GetMembers().OfType<IPropertySymbol>()
                .Where(prop => !prop.IsStatic)
                .Where(prop => prop.DeclaredAccessibility == Accessibility.Public)
                .Where(prop => prop.IsReadOnly);
        }

        public static async Task<ConstructorDeclarationSyntax> CreateConstructor(this INamedTypeSymbol typeSymbol)
        {
            if (typeSymbol == null) throw new ArgumentNullException(nameof(typeSymbol));
            var props = typeSymbol.GetReadOnlyProperties().ToArray();
            if (props.Length == 0) throw new ArgumentException(nameof(typeSymbol) + " is not a valid record type");
            var propSyntaxes =  await Task.WhenAll(props.Select(prop => prop.DeclaringSyntaxReferences.Single().GetSyntaxAsync()));

            ParameterListSyntax parameters =
                SyntaxFactory.ParameterList(
                    SyntaxFactory.SeparatedList(
                        propSyntaxes.Cast<PropertyDeclarationSyntax>().Select(propSyntax =>
                        {
                            return SyntaxFactory.Parameter(
                                SyntaxFactory.List<AttributeListSyntax>(),
                                SyntaxFactory.TokenList(),                                
                                propSyntax.Type,
                                SyntaxFactory.Identifier(propSyntax.Identifier.ValueText.ToFirstCharLower()),
                                null);
                        })));

            return
                SyntaxFactory.ConstructorDeclaration(SyntaxFactory.Identifier(typeSymbol.Name))
                .WithModifiers(PublicModifiers)
                .WithParameterList(parameters)
                .WithBody(SyntaxFactory.Block(props.Select(PropertyInitExpression).ToArray()));
        }

        private static ExpressionStatementSyntax PropertyInitExpression(IPropertySymbol prop)
        {
            return
                SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(prop.Name),
                        SyntaxFactory.IdentifierName(prop.Name.ToFirstCharLower())));
        }
    }  
    
     
}
