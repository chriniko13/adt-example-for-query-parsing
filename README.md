### ADT Example for Query Parsing
#

#### Description

Just a sample Algebraic Data Type in order to parse a query in a format that front end application
sends to back end.

```text
   TreeFormatRule = 
        TreeFormatNestedRule(id * frequency * combinator * [TreeFormatRule] * not) 
        + 
        TreeFormatFlatRule(id * field * operator * value)
        
        
        
   TreeFormatFlatRuleValue = 
        TreeFormatFlatRuleSingleValue(value) 
        + 
        TreeFormatFlatRuleMultiValue([value])

```

The query tool used in front end is the following: https://github.com/sapientglobalmarkets/react-querybuilder


#### Unit Tests
In order to run them, execute: `sbt test`