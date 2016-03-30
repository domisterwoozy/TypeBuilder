using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeBuilder
{
    public static class Util
    {
        public static string ToFirstCharLower(this string str)
        {
            if (str == null) throw new ArgumentNullException(nameof(str));
            if (str.Length == 0) return "";
            if (str.Length == 1) return str.ToLowerInvariant();
            return char.ToLowerInvariant(str[0]) + str.Substring(1);
        }

        public static string ToFirstCharUpper(this string str)
        {
            if (str == null) throw new ArgumentNullException(nameof(str));
            if (str.Length == 0) return "";
            if (str.Length == 1) return str.ToUpperInvariant();
            return char.ToUpperInvariant(str[0]) + str.Substring(1);
        }

        public static IEnumerable<T> Concat<T>(this IEnumerable<T> enumerable, T singleItem)
        {
            if (enumerable == null) throw new ArgumentNullException(nameof(enumerable));
            foreach (var item in enumerable) yield return item;
            yield return singleItem;
        }

        public static IEnumerable<T> Concat<T>(this T singleItem, T otherItem)
        {
            if (singleItem == null) throw new ArgumentNullException(nameof(singleItem));
            yield return singleItem;
            yield return otherItem;
        }

        public static IEnumerable<T> Concat<T>(this T singleItem, IEnumerable<T> enumerable)
        {
            if (singleItem == null) throw new ArgumentNullException(nameof(singleItem));
            yield return singleItem;
            foreach (var item in enumerable) yield return item;
        }
    }
}
