using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using TestHelper;
using TypeBuilder;

namespace TypeBuilder.Test
{
    [TestClass]
    public class UnitTest : CodeFixVerifier
    {

        //No diagnostics expected to show up
        [TestMethod]
        public void EmptyTest()
        {
            var test = @"";

            VerifyCSharpDiagnostic(test);
        }

        //Diagnostic and CodeFix both triggered and checked for
        [TestMethod]
        public void EmployeeTest()
        {
            const string test = 
@"namespace TypeBuilder.Test
{
    public class Employee
    {
        public string Name { get; }
        public string Address { get; }
        public string Title { get; }
        public int Age { get; }

        public Employee(string name, string address, string title, int age)
        {
            Name = name;
            Address = address;
            Title = title;
            Age = age;
        }
    }
}";
            var expected = new DiagnosticResult
            {
                Id = "TypeBuilder",
                Message = string.Format("Type name '{0}' can be given a builder.", "Employee"),
                Severity = DiagnosticSeverity.Info,
                Locations =
                    new[] {
                            new DiagnosticResultLocation("Test0.cs", 3, 18)
                        }
            };

            VerifyCSharpDiagnostic(test, expected);

            const string fixtest = 
@"namespace TypeBuilder.Test
{
    public class Employee
    {
        public string Name { get; }
        public string Address { get; }
        public string Title { get; }
        public int Age { get; }

        public Employee(string name, string address, string title, int age)
        {
            Name = name;
            Address = address;
            Title = title;
            Age = age;
        }

        public class Builder
        {
            public string Name { get; set; }
            public string Address { get; set; }
            public string Title { get; set; }
            public int Age { get; set; }

            public Employee Build()
            {
                return new Employee(Name, Address, Title, Age);
            }
        }
    }
}";
            VerifyCSharpFix(test, fixtest);
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            return new TypeBuilderCodeFixProvider();
        }

        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new TypeBuilderAnalyzer();
        }
    }
}