---
title: How to Navigate the Scaladoc Interface (2.10/2.11)
author: sanjiv sahayam
description: The Scaladoc 2.10/2.11 interface has a lot of features jammed-packed into it. This guide helps illuminate the features that will make you more effective.
tags: scala, scaladoc
comments: true
---

It might be surprising to see a "how to" for navigating the [Scaladoc](http://docs.scala-lang.org/overviews/scaladoc/overview) interface. Since I've started [using Sublime for Scala development](http://sanj.ink/posts/2015-07-15-using-sublime-for-scala-development.html) I've come to heavily rely on the Scaladoc for any library I'm using. I've come across a few nuances that might be hard to notice at first glance. This post is my attempt to document those nuances and a little guide on how to be more effective in navigating the generated documentation.

The Scaladoc interface is teeming with features that don't seem to have any documentation. We need documentation for our documentation system. How meta! That said, [Dick Wall](https://twitter.com/dickwall) has recorded a screencast called [Using the Scaladoc Interface](http://docs.scala-lang.org/overviews/scaladoc/interface.html) which you should definitely checkout.

This post will cover some of the features he highlights plus a few more.

- [Website Layout](#website-layout)
- [Searching](#searching)
- [Displaying Results](#displaying-results)
    - [Matched Content Area](#matched-content-area)
        - [Member Search Bar](#member-search-bar)
        - [Member Definition Area](#member-definition-area)
            - [1. Type Members](#type-members)
            - [2. Concrete Value Members](#concrete-value-members)
            - [3. Abstract Value Members](#abstract-value-members)
            - [4. Instance Constructors](#instance-constructors)
            - [5. Implicit Value Members](#implicit-value-members)
            - [6. Shadowed Implicit Value Members](#shadowed-implicit-value-members)
- [Miscellaneous](#miscellaneous)

# Website Layout

The Scaladoc website is separated into 2 main windows:

1. The Search Window.
2. The Results Window.

![Scaladoc Website Layout](/images/scaladoc_navigate/scaladoc_interface_basics_layout_panels.png)

Clicking on the _Layout Toggle Button_ (1) allows you to hide the _Search Window_ and focus only on the _Results Window_. You can also drag the divider to change the window sizes.

![Layout Toggle Button](/images/scaladoc_navigate/scaladoc_interface_basics_layout_toggle_button.png)

These windows can be broadly subdivided into three main components:

![Scaladoc Website Layout](/images/scaladoc_navigate/scaladoc_interface_basics.png)

1. The _Entity Search Bar_ on the top left. This allows you to search by top-level entities such as packages, classes, traits and objects. Nested or [path-dependent types](http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html), methods and variables (val and var) and any entity with symbolic characters in its name can't be searched from here.
2. The _Search Results List_ found just below the _Entity Search Bar_, which displays any matched results based your query.
3. The _Matched Content Area_ in the _Results Window_ displays a selected search result in more detail.

__NB__: _The names for the various areas mentioned above are my own. I have been unable to find any documentation on what these areas on the website are actually called. If you know the actual names of these areas in the interface, please let me know and I will update this post_.

This is a simplified view highlighting the main areas. We'll go into detail about the other areas later.

# Searching

Searching is primarily done within the _Search Window_. To find an entity such as a class, you can type in the name of the class into the _Entity Search Bar_ and the _Search Results List_ will display only matching results, grouped by the packages they are in.

1. The search has both case-sensitive and case-insensitive functionalities. If you start the search with an uppercase letter, the search is case-sensitive from that point onwards. In the example below, we search for __BitSet__ with the exact case and we get a number of matches (1). If we don't use the correct case we get no matches (2).

![Matching on Case](/images/scaladoc_navigate/scaladoc_interface_basics_entity_search_area_case_sensitive_match.png)

If you keep all the letters lowercase, then the search is case-insensitive. In the example below, the search is all lowercase characters and results in the same matches as that of the exact case (1).

![Match with lowercase](/images/scaladoc_navigate/scaladoc_interface_basics_entity_search_area_case_insensitive_match.png)

2. The search is a wildcard and will match any top-level entity name with that text in it, including packages.

![Wildcard Search](/images/scaladoc_navigate/scaladoc_interface_basics_wildcard_search.png)

3. You can also match on mnemonics of your class name. In the example below we use __LHS__ to find __LinkedHashSet__ class.

![Mnemonic Search](/images/scaladoc_navigate/scaladoc_interface_basics_camel_case_search.png)

4. If you search by entering a package path, the results will be filtered to include only the specified package and sub packages.

![Filter by Package-Path](/images/scaladoc_navigate/scaladoc_interface_basics_package_path_search.png)


5. You can also search by typing in the full package path to the entity.

![Filter by Full Package-Path](/images/scaladoc_navigate/scaladoc_interface_basics_full_package_path.png)

If you don't want to type in the full package path to an entity, you can simply use the last subpackage of its package path along with the entity. In the example below we search for __scala.collection.immutable.Seq__ by searching for __immutable.Seq__.

![Package-Path Shorter Filter](/images/scaladoc_navigate/scaladoc_interface_basics_package_path_subpackage_search.png)

6. Clicking on the __focus__ link on a package header in the _Search Results List_ will constrain the search to only include entities within the focussed package.

![Package-Path Focus](/images/scaladoc_navigate/scaladoc_interface_basics_package_focus_button.png)

In the example below we focus on __scala.collection.immutable__ and then search for __List__. The results are limited to matches in the __scala.collection.immutable__ package.

![Package-Path Filter](/images/scaladoc_navigate/scaladoc_interface_basics_package_focus.png)

7. Clicking on the __display packages only__ filter will only show packages in the _Search Results List_. If a search term is in the _Entity Search Bar_, then only packages matching the search term and packages with entities that match the search term will be displayed.
![Display Packages Only](/images/scaladoc_navigate/scaladoc_interface_basics_display_package_only_button.png)

Clicking on the __display all entities__ filter clears the package filter.

![Filtered Packages Only](/images/scaladoc_navigate/scaladoc_interface_basics_display_package_only.png)

8. When displaying only packages, use the __show__ and __hide__ filter on the package header to expand and collapse entities within that package, respectively.

![Show and Hide Filters](/images/scaladoc_navigate/scaladoc_interface_basics_show_hide_packages.png)

9. Pressing __ESC__ clears any search terms in the _Entity Search Bar_.

# Displaying Results

Once we have some matches in the _Search Results List_, we can open them in a number of ways:

1. Clicking directly on the package header will open the package documentation for that package in the _Matched Content Area_.

![Package Documentation](/images/scaladoc_navigate/scaladoc_interface_basics_package_doc.png)

2. Clicking on the name of a class, object or trait will open its documentation in the _Matched Content Area_. Clicking on the class (1), object (2) or trait (3) icon will open the corresponding entity documentation in the _Matched Content Area_.

![Class, Traits and Objects](/images/scaladoc_navigate/scaladoc_interface_basics_classes_traits_objects.png)

3. After entering a search in the _Entity Search Bar_, [pressing the down arrow will allow you to navigate through the Search Results List](http://heather.miller.am/scaladoc-keyboard-shortcuts.html). Pressing enter on any result will open its documentation in the _Matched Content Area_.

4. Clicking on the __deprecated__ filter in the _Search Window_, will open up the _Matched Content Area_ with all the deprecated members in the api. Members include types, path-dependent types, methods, implicits and variables.

![Deprecated Members](/images/scaladoc_navigate/scaladoc_interface_basics_deprecated_entities.png)

5. The clicking on any letter on the alpha bar (1) will open up a list of all members starting with that letter in the _Matched Content Area_.

![Alpha Bar](/images/scaladoc_navigate/scaladoc_interface_basics_alphabar.png)

In the example below, we click on the letter __T__ to search for the path-dependent type __TypeSymbol__.

![Filtering by Letters](/images/scaladoc_navigate/scaladoc_interface_basics_filter_by_letter.png)


6. Clicking on the __#__(symbol) filter (1) will open up a list of all members with special-charactered names in the api.

![Symbol Filter](/images/scaladoc_navigate/scaladoc_interface_basics_symbol_filter.png)

![Filtering by Symbol](/images/scaladoc_navigate/scaladoc_interface_basics_filter_by_symbol.png)

Next lets look at the _Matched Content Area_ in more detail.

## Matched Content Area

There's quite a lot of detail in this area. Here's an overview of some items of interest.

1. The entity type is shown on the top left.

![Entity Type](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_entity_type.png)

There are five different icons for packages, classes, traits, objects and path-dependent types. If any types other than packages, have a related companion object or extractor, or a companion object or extractor has a related type, then this relationship is depicted by the _sticker peal_ overlay. Clicking on the sticker peal overlay takes you to the companion object or extractor from the type or to the type from the companion object or extractor. Here are the icons mentioned in the above scenarios:

![Package](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_package_icon.png)

![Class](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_classicon.png)

![Class with Companion or Extractor](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_class-with-companion_icon.png)

![Trait](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_trait_icon.png)

![Trait with Companion or Extractor](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_trait-with-companion_icon.png)

![Object](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_object_icon.png)

![Object with Related Type](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_object_with_related_type_icon.png)

![Path-dependent Type](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_path-dependent-type.png)

![Path-dependent Type with Companion or Extractor](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_path-dependent-type-wth-companion.png)

2. The package breadcrumb path allows you to click through to any sub package for a type.

![Package Breadcrum Path](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_package_breadcrumbs.png)

3. Any traits or classes extended by the type. You can navigate to any of these super types through their hyperlinks.

![Extensions](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_extensions.png)

4. If this type has a companion object or extractor, then similar to clicking on the sticker peal icon, this link will take you to the companion object or extractor. If this is the companion object or extractor of a related type, this link will take you back to the related type.

![Object Link](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_object_link.png)

5. Link to the package documentation for this type.

![Package Link](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_package_link.png)

6. The entity documentation. This is usually an overview of the type and what it is used for. There might be some handy example usages in there as well.

![Entity Documentation](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_entity_documentation.png)

7. Any linear supertypes or the order in which supertypes are resolved. From [Scala Design Patterns](https://www.safaribooksonline.com/library/view/scala-design-patterns/9781785882500/ch02s04.html):

> traits offer a form of multiple inheritance. In such cases, the hierarchy is not necessarily linear, but forms an acyclic graph that needs to be flattened upon compilation. What linearization does is this: it specifies a single linear order for all of the ancestors of a class, including both the regular superclass chain and the parent chains of all of the traits.

![Linear Supertypes](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_linear_supertypes.png)

8. All known subclasses with handy hyperlinks.

![Known Subclasses](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_known_subclasses.png)

9. Link to the source file where this entity is defined. This is one of the most useful features of Scaladoc. :)

![The Source Link](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_source_file.png)

10. The _Member Search Bar_ will let you search within the _Matched Content Area_ for available members.

![Member Search Bar](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_member_search_bar.png)

11. Filters that can be applied to the results.

    ![Content Filters](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_content_filters.png)


    * __Ordering__ lets you order the results __Alphabetically__ or __By Inheritance__ or by a __Grouped__ category. Alphabetic ordering simply orders all members alphabetically (a-z) within each member section. Ordering by inheritance, orders members by which supertype they are defined on. Grouped orders members across some predefined grouping created by the Scaladoc author. See [scala.reflect.api.Symbols](http://www.scala-lang.org/api/2.11.6/scala-reflect/index.html#scala.reflect.api.Symbols) for an example of this.

    * __Inherited__ allows toggling members defined in any combination of supertypes. This is useful to find out where a method is defined when you have a complex inheritance hierarchy.

    * __Implicitly__ defines which additional methods are available via implicits defined in scope.

    * __Visibility__ restricts members to either __Public__ modifier scopes or __All__ scopes other than private.

12. Defines various member definitions sections. See the [Member Definition Area](#member-definition-area) for more details.

![Member Definition Area](/images/scaladoc_navigate/scaladoc_interface_basics_matched_content_area_overview_member_definition_area.png)

13. The package documentation for a package, has a Content Hierarchy diagram that describes the relationship between the types within that package.

![Content Hierarchy Diagram](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_content_hierarchy.png)

14. Types that have been annotated with a @[contentDiagram tag](http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html#diagram-tags), generate a Type Hierarchy that describes the relationships between a type and its super and sub types.

![Type Hierarchy Diagram for scala.reflect.api.Symbols](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_type_hierarchy.png)

### Member Search Bar

To find a member, type in a query into the _Member Search Bar_.

1. The search is completely case-insensitive; Content is matched irrespective of case.
![Case-insensitive Member Search](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_member_search_bar_case_insensitive.png)
2. You can't use a mnemonic search like in the _Entity Search Bar_.
3. You can search on any text in a member _description_. In the example below we search for the text __w.r.t__ and matches are found in member descriptions.

![Text Search within Member Descriptions](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_member_search_bar_text_search.png)

4. You can't directly search against types in method signatures for example. They have to be mentioned in the description for a match to be found. In the example below we search for __GenTraversable__ within the __scala.collection.immutable.Seq__ documentation. While __GenTraversable__ is used in method definitions there are no matches returned from the search because __GenTraversable__ is not mentioned in any _descriptions_.

![Type Name Not Found](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_member_search_bar_no_type_search.png)

If we perform a browser search on the same page for __GenTraversable__ we find search hits.

![Type Found in Browser Search](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_member_search_bar_type_by_browser_search.png)

4. You can tab between the _Member Search Bar_ and the _Entity Search Bar_. I'm not sure how useful this really is but it's there if you need it.

5. Pressing __ESC__ clears any search terms in the _Member Search Bar_.

6. You can't only search for a __def__ for example and exclude other members such as a __val__. Searches of the type:

```{.scala .scrollx}
def [method_name]
```

yield nothing. The browser search doesn't match queries like the above either. As an example, if we search for the following through Chrome:

```{.scala .scrollx}
def collect
```

on the members of __scala.collection.immutable.Seq__, we don't find any matches. This is annoying as the __collect__ method is clearly defined in the documentation.

### Member Definition Area

There is some commonality between the features available to the different member types. Many of the features available in one member type can be found in other types as well.

Member definitions can be separated into six areas.

#### 1. Type Members

Path-dependent types (nested types).

![Type Members](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_type_members.png)

1. The name of the path-dependent type. Although there is no visible hyperlink to the the class name (underline), it is in most cases a hyperlink and can be clicked to go through to the type's documentation. This had me confused for a while.
2. The summary of what the type is about.
3. Any other types extended by this type. All supertypes are linked and can be navigated.
4. Dropping this arrow down will reveal some additional information about this type.

![Type Member Expanded](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_type_members_dropped.png)

5. The parent type where this nested type is defined.

#### 2. Concrete Value Members

Members that have an implementation.

![Concrete Value Members](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_concrete_value_members_dropped.png)

1. The member definition, with parameter and result types hyperlinked.
2. Description of the method, including parameters, types and results.
3. Full signature. This is present if the member definition (1) is simplified, which can be identified by the __[[use case]]__ ascription. In the above case, the member definition (1) does not mention the __CanBuildFrom__ implicit which is shown in the full signature.
4. Where in the inheritance hierarchy this method is defined. In the above case the __++__ method is defined in __GenTraversableLike__ and then overridden in __TraversableLike__.
5. [Permalink to this member](http://www.scala-lang.org/api/2.11.8/index.html#scala.collection.GenTraversableLike@++[B](that:scala.collection.GenTraversableOnce[B]):scala.collection.GenTraversable[B]), which embeds the method signature in a URL which can be used to link straight to __++__ method documentation.

```{.command .scrollx}
http://www.scala-lang.org/api/2.11.8/index.html#scala.collection.GenTraversableLike@++[B](that:scala.collection.GenTraversableOnce[B]):scala.collection.GenTraversable[B]
```
#### 3. Abstract Value Members

Members that are abstract (1) (have no implementation).

![Abstract Value Members](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_abstract_value_members_dropped.png)


#### 4. Instance Constructors

Constructors for creating an instance of the type.

![Instance Constructors](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_instance_constructors.png)


#### 5. Implicit Value Members

Implicit members:

1. Implicit types (usually  classes) are defined with the Type members.
2. Implicit values (__var__, __val__ and __def__) are defined with the Value members.

![Implicit Members](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_implicit_value_members.png)


#### 6. Shadowed Implicit Value Members

Implicit members that are not applicable due to existing implementations.

![Shadowed Implicits Value Members](/images/scaladoc_navigate/scaladoc_interface_basics_member_definition_area_shadowed_implicit_value_members_dropped.png)

1. Member supplied by an implicit conversion.
2. Where implicit comes from and when it gets applied.
3. How to use the shadowed implicit (see below for an explanation) by using a type ascription.

Implicit conversions can be used to add methods to a type which doesn't have those particular methods ("pimping"). If the target type already has a method with the same signature (name and types) that the implicit conversion is trying to add to it, it is termed "shadowing". The definition of the method on the target type takes precedence over the implicit conversion which is not applied. A type ascription can be used to manually specify which method definition to use.

From [What is "Shadowed Implicit Value Members" in Scala doc?](http://stackoverflow.com/questions/19615810/what-is-shadowed-implicit-value-members-in-scala-doc):

```{.scala .scrollx}
case class Foo(x :String) {
  def whoAmI = "Foo: " + x
}

implicit class Bar(foo: Foo) {
  def whoAmI = "Bar: " + foo.x
}

println( Foo("test").whoAmI ) //prints Foo: test
println( (Foo("test"): Bar).whoAmI ) //prints Bar: test
```

# Miscellaneous

1. If you know the package path of the member you are looking for, you can append it to the documentation URL with a __#__ to go directly to the documentation. In the example below we go directly to the __scala.concurrent.Future__ documentation by adding it to the Scaladoc API URL:

```{.scala .scrollx}
"http://www.scala-lang.org/api/2.11.8/index.html" +
"#" +
"scala.concurrent.Future"
```

The above link takes you to the [Future type](http://www.scala-lang.org/api/2.11.8/index.html#scala.concurrent.Future)  documentation.

If you want to go to the [Future object](http://www.scala-lang.org/api/2.11.8/index.html#scala.concurrent.Future$), then add __$__ to the end of the path to the type documentation.

```{.scala .scrollx}
"http://www.scala-lang.org/api/2.11.8/index.html" +
"#" +
"scala.concurrent.Future" +
"$"
```

If you want to go to a path-dependent type, then use the above formula and use a __$__ between the parent type and the nested type. In the example below if we want to go the documentation for [TypeSymbol](http://www.scala-lang.org/api/2.11.1/scala-reflect/index.html#scala.reflect.api.Symbols\$TypeSymbol) which is a nested type of __scala.reflect.api.Symbols__ we would write the link as:

```{.scala .scrollx}
"http://www.scala-lang.org/api/2.11.1/scala-reflect/index.html" +
"#" +
"scala.reflect.api.Symbols" +
"$" +
"TypeSymbol"
```

This unfortunately doesn't work as expected for all types. The location of the documentation for a member depends on where it is defined. Taking __scala.collection.immutable.Set__ as an example, if we want to see its __WithFilter__ path-dependent type's documentation, we might incorrectly assume the documentation URL is:

```{.scala .scrollx}
"http://www.scala-lang.org/api/2.11.8/index.html" +
"#" +
"scala.collection.immutable.Set" +
"$" +
"WithFilter"
```

But the [above](http://www.scala-lang.org/api/2.11.8/index.html#scala.collection.immutable.Set\$WithFilter) doesn't work because __WithFilter__ is defined on __scala.collection.TraversableLike__. This [link](http://www.scala-lang.org/api/2.11.8/index.html#scala.collection.TraversableLike$WithFilter) works:

```{.scala .scrollx}
"http://www.scala-lang.org/api/2.11.8/index.html" +
"#" +
"scala.collection.TraversableLike" +
"$" +
"WithFilter"
```

Also as mentioned previously, you can bookmark a permalink to any member and use that to go directly to any documentation.

That about covers it for the overview of the Scaladoc interface. If I've missed any useful tips or misunderstood a feature, please let me know in the comments section below.
