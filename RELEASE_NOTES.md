### 2.1.0 - March 7 2022
- add tuple differ

### 2.0.0 - March 6 2022
- support netstandard2.0 or higher
- drop PCL, netstandard1.6 and net4.5 or lower
- update FSharp.Core(5.0.0)

### 1.0.0 - April 7 2017
- support .NET Core
- return obj type when instances have different types

### 0.10.0 - November 13 2016
- fix accessibility of CollectionItemAccessor#TryGet
- avoid infinite loop

### 0.9.0 - September 4 2016
- fix typo [#2](https://github.com/pocketberserker/FSharp.Object.Diff/pull/2)
- fix assembly title

### 0.8.0 - June 29 2016
- fix target FSharp.Core version

### 0.7.0 - June 29 2016
- support PCL

### 0.6.1 - June 2 2016
- fix get ICollection item

### 0.6.0 - June 1 2016
- fix IdentityService API
- fix check collection index
- try get indexed value

### 0.5.3 - May 28 2016
- fix priority of differ

### 0.5.2 - May 24 2016
- fix collection type check
- fix copy index

### 0.5.1 - May 24 2016
- reverts commit fe9e711fca53bed5157e976e061f8683bdaf2be0

### 0.5.0 - May 24 2016
- show collection index in NodePath

### 0.4.0 - May 24 2016
- reimplement collection differ and collection accessor

### 0.3.1 - May 23 2016
- fix type check(string is not simple type)

### 0.3.0 - May 22 2016
- change accss modifier
- fix Instances#TryGetWork, Instances#TryGetWorking
- remove string type check from CollectionDiffer
- reimplement dictionary differ and dictionary accessor

### 0.2.0 - May 18 2016
- fix copy target collection in CollectionDiffer
- fix API in ComparisonService#ResolveResolveComparisonStrategy
- require property name (EqualsOnlyComparisonStrategy, ObjectDiffPropertyAttribute, ObjectDiffEqualsOnlyAttribute)
- fix typo (ComparisonConfigurer#OfType and OfPrimitiveTypes)
- fix stop loop when raise StopVisitationException
- fix equality (CollectionItemElementSelector)
- fix get value from collection

### 0.1.1 - May 17 2016
- fix check removed in primitive differ

### 0.1.0 - May 17 2016
- inital release
