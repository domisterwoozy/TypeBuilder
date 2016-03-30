using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace TypeBuilder
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class TypeBuilderAnalyzer : DiagnosticAnalyzer
    {
        private const string Category = "CodeGeneration";

        public const string DiagIDBuilder = "TypeBuilder";

        private static readonly LocalizableString Title =
            new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat =
            new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description =
            new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        

        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagIDBuilder, Title, MessageFormat, Category, DiagnosticSeverity.Info, true, Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSymbolAction(AnalyzeSymbol, SymbolKind.NamedType);
        }

        private static void AnalyzeSymbol(SymbolAnalysisContext context)
        {
            var namedTypeSymbol = (INamedTypeSymbol)context.Symbol;

            if (!IsRecordCandidate(namedTypeSymbol)) return;            

            // For all such symbols, produce a diagnostic.
            var diagnostic = Diagnostic.Create(Rule, namedTypeSymbol.Locations[0], namedTypeSymbol.Name);
            context.ReportDiagnostic(diagnostic);            
        }

        // a type is buildable if it has 1 or more readonly properties
        private static bool IsRecordCandidate(INamedTypeSymbol typeSymbol) => typeSymbol.GetReadOnlyProperties().Any();

         
    }
}
