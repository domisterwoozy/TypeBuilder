using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Formatting;

namespace TypeBuilder
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(TypeBuilderCodeFixProvider)), Shared]
    public class TypeBuilderCodeFixProvider : CodeFixProvider
    {
        private const string constrTitle = "Generate record constructor";
        private const string builderTitle = "Generate type builder";
        private const string withTitle = "Generate with methods";
        private const string equalsTitle = "Generate equals/hashcode/tostring methods";

        private enum FixType { Builder, With, Equals }

        public sealed override ImmutableArray<string> FixableDiagnosticIds => ImmutableArray.Create(TypeBuilderAnalyzer.DiagIDBuilder);        

        public sealed override FixAllProvider GetFixAllProvider() => WellKnownFixAllProviders.BatchFixer;        

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            foreach(var diag in context.Diagnostics)
            {
                if (diag.Id != TypeBuilderAnalyzer.DiagIDBuilder) throw new InvalidOperationException("Invalid diagnostic ID");

                var diagnosticSpan = diag.Location.SourceSpan;
                // Find the type declaration identified by the diagnostic.
                var typeDecl = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();
                // Get the symbol representing the type to be renamed.
                var semanticModel = await context.Document.GetSemanticModelAsync();
                var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl);
                // Register a code action that will invoke the fix.

                var validConstr = GetValidConstructor(typeSymbol);
                if (validConstr.HasFailed)
                {
                    context.RegisterCodeFix(CodeAction.Create(constrTitle, c => MakeConstructorAsync(context.Document, root, typeSymbol, typeDecl, c), constrTitle), diag);
                    return;
                }

                if (!HasBuilder(typeSymbol))
                {
                    context.RegisterCodeFix(CodeAction.Create(builderTitle, c => MakeBuilderAsync(context.Document, root, typeDecl, FixType.Builder, validConstr.Item, c), builderTitle), diag);
                }
                if (!HasWith(typeSymbol))
                {
                    context.RegisterCodeFix(CodeAction.Create(withTitle, c => MakeBuilderAsync(context.Document, root, typeDecl, FixType.With, validConstr.Item, c), withTitle), diag);
                }
                if (!HasEquals(typeSymbol))
                {
                    context.RegisterCodeFix(CodeAction.Create(equalsTitle, c => MakeBuilderAsync(context.Document, root, typeDecl, FixType.Equals, validConstr.Item, c), equalsTitle), diag);
                }
            }            
        }

        private async Task<Solution> MakeConstructorAsync(Document document, SyntaxNode root, INamedTypeSymbol typeSymbol, TypeDeclarationSyntax typeDecl, CancellationToken cancellationToken)
        {
            // Produce a new solution that has all references to that type renamed, including the declaration.
            var originalSolution = document.Project.Solution;
            var optionSet = originalSolution.Workspace.Options;

            var newNode = await typeSymbol.CreateConstructor();
            SyntaxNode newRoot = root.InsertNodesAfter(typeDecl.ChildNodes().Last(), new[] { newNode });
            Document newDoc = document.WithSyntaxRoot(newRoot);
            Document formattedDoc = await Formatter.FormatAsync(newDoc, optionSet);
            SyntaxNode formattedRoot = await formattedDoc.GetSyntaxRootAsync();
            Solution newSolution = document.Project.Solution.WithDocumentSyntaxRoot(document.Id, formattedRoot);

            return newSolution;
        }

        private async Task<Solution> MakeBuilderAsync(Document document, SyntaxNode root, TypeDeclarationSyntax typeDecl, 
            FixType fixType, IMethodSymbol validConstructor, CancellationToken cancellationToken)
        {
            // Produce a new solution that has all references to that type renamed, including the declaration.
            var originalSolution = document.Project.Solution;
            var optionSet = originalSolution.Workspace.Options;

            var completeMap = await RecordBuilder.CreateMap(typeDecl, validConstructor);

            var nodesToAdd = new List<SyntaxNode>();
            switch (fixType)
            {
                case FixType.Builder:
                    nodesToAdd.Add(completeMap.CreateBuilder());
                    break;
                case FixType.With:
                    nodesToAdd.AddRange(completeMap.PropertySymbols.Select(prop => completeMap.CreateWith(prop)));
                    break;
                case FixType.Equals:
                    nodesToAdd.AddRange(completeMap.CreateEqualityMethods());
                    break;
                default:
                    break;
            }

            SyntaxNode newRoot = root.InsertNodesAfter(typeDecl.ChildNodes().Last(), nodesToAdd);
            Document newDoc = document.WithSyntaxRoot(newRoot);      
            Document formattedDoc = await Formatter.FormatAsync(newDoc, optionSet);
            SyntaxNode formattedRoot = await formattedDoc.GetSyntaxRootAsync();
            Solution newSolution = document.Project.Solution.WithDocumentSyntaxRoot(document.Id, formattedRoot);

            return newSolution;
        }

        private static Failable<IMethodSymbol> GetValidConstructor(INamedTypeSymbol typeSymbol)
        {
            var readOnlyProps = typeSymbol.GetReadOnlyProperties().ToArray();
            if (readOnlyProps.Length == 0) return Failable<IMethodSymbol>.Failed("Type contains zero read only properties");
            if (readOnlyProps.Any(prop => prop.Name.Length == 0 || !char.IsUpper(prop.Name[0])))
            {
                return Failable<IMethodSymbol>.Failed("Type contains read only properties that do not start with an upper case char");
            }
            var propNames = new HashSet<string>(readOnlyProps.Select(prop => prop.Name));

            var validConstructors =
                typeSymbol.InstanceConstructors.Where(c => c.Parameters.Length == readOnlyProps.Length)
                .Where(c => c.Parameters.All(p => char.IsLower(p.Name[0])))
                .Where(c => c.Parameters.All(p => readOnlyProps.SingleOrDefault(prop => prop.Name == p.Name.ToFirstCharUpper())?.Type?.Equals(p.Type) ?? false)) // do matching names have matching types
                .ToArray();

            
            if (validConstructors.Length == 0) return Failable<IMethodSymbol>.Failed("No valid constructors exist.");
            if (validConstructors.Length > 1) return Failable<IMethodSymbol>.Failed("Multiple valid constructors exist.");
            return Failable<IMethodSymbol>.Success(validConstructors[0]);
        }

        private bool HasBuilder(INamedTypeSymbol typeSymb) => typeSymb.GetMembers().OfType<INamedTypeSymbol>().Select(namedType => namedType.Name).Contains("Builder");
        private bool HasWith(INamedTypeSymbol typeSymb) => typeSymb.GetMembers().OfType<IMethodSymbol>().Any(m => m.Name.StartsWith("With"));
        private bool HasEquals(INamedTypeSymbol typeSymb) => typeSymb.GetMembers().OfType<IMethodSymbol>().Any(m => m.Name == "Equals");


    }
}