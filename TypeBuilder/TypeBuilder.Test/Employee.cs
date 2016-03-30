using System;

namespace TypeBuilder.Test
{
    public class Employee : IEquatable<Employee>
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

        public Employee WithName(string name) => new Employee(name, Address, Title, Age);

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


        public bool Equals(Employee other)
        {
            throw new NotImplementedException();
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Employee)) return false;
            var typedObj = (Employee)obj;
            if (!Name.Equals(typedObj.Name)) return false;
            if (!Address.Equals(typedObj.Address)) return false;
            return true;
        }

        public override int GetHashCode()
        {
            int hashCode = 17;
            hashCode = hashCode * 23 + Name.GetHashCode();
            hashCode = hashCode * 23 + Address.GetHashCode();
            return hashCode;
        }

        public override string ToString()
        {
            return $"Name: '{Name}', Address: '{Address}', Title: '{Title}', Age: '{Age}'";
        }


    }
}
