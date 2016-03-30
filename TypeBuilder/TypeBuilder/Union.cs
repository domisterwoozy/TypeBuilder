using System;

namespace TypeBuilder
{
    public abstract class Union<A, B>
    {
        public abstract T Match<T>(Func<A, T> f, Func<B, T> g);

        private Union() { }

        public sealed class CaseOne : Union<A, B>
        {
            public readonly A Item;
            public CaseOne(A item) : base() { Item = item; }
            public override T Match<T>(Func<A, T> f, Func<B, T> g) => f(Item);
        }

        public sealed class CaseTwo : Union<A, B>
        {
            public readonly B Item;
            public CaseTwo(B item) : base() { Item = item; }
            public override T Match<T>(Func<A, T> f, Func<B, T> g) => g(Item);
        }
    }

    public struct Failable<T>
    {
        private readonly T item;
        public bool HasFailed { get; }
        public string FailureMessage { get; }
        public T Item
        {
            get
            {
                // can't decide whether to jsut return the default or throw an exception
                // your not supposed to throw in a getter but Nullable<T> does this and it makes more sense
                if (HasFailed) throw new InvalidOperationException("Cannot retrieve item when it has failed");
                else return item;
            }
        }

        private Failable(bool failed, string message, T item)
        {
            HasFailed = failed;
            FailureMessage = message;
            this.item = item;
        }

        public static Failable<T> Success(T item) => new Failable<T>(false, "", item);
        public static Failable<T> Failed(string message) => new Failable<T>(true, message, default(T));
    }
}
