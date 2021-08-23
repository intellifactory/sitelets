using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Sitelets;

namespace ANCCSharp.SiteletsTest
{
    public class TestSitelet
    {
        public static Sitelet<object> S =>
        new SiteletBuilder()
            .With("/sitelets", ctx =>
                  "Hello World from C#"
            )
            .Install();
    }
}
