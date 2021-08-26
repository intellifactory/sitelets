using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Sitelets;

namespace ANCCSharp.SiteletsTest
{
    [EndPoint("{first}/{last}")]
    public class Name
    {
        public string first;
        public string last;
    }

    [EndPoint("/person/{name}/{age}", "/person/{age}/{name}")]
    public class Person
    {
        public Name name;
        public int age;
    }

    [Method("GET"), EndPoint("qperson/{name}")]
    public class QueryPerson
    {
        public QueryName name;

        [Query]
        public int? age;
    }

    public class QueryName
    {
        [Query]
        public string first;

        [Query]
        public string last;
    }

    public class TestSitelet
    {
        public static Sitelet<object> S =>
        new SiteletBuilder()
            .With("/hello", ctx =>
                  "Hello World from C#"
            )
            .With<Person>((ctx, person) =>
                String.Format("<p>{0} {1} is {2} years old.</p>", person.name.first, person.name.last, person.age)
            )
            .With<QueryPerson>((ctx, person) =>
                person.age.HasValue ?
                    String.Format("<p>{0} {1} is {2} years old.</p>", person.name.first, person.name.last, person.age.Value) :
                    String.Format("<p>{0} {1} won't tell their age.</p>", person.name.first, person.name.last)
            )
            .Install();
    }
}
