/* ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------*/

Table.create("books_range", "Id", "N", Some("Title"), Some("S"))

val i1 = new Item(
  ("Id", "N", "101"),
  ("Title", "S", "Some Title"),
  ("ISBN", "S", "ABC"))

val i2 = new Item(
  ("Id", "N", "102"),
  ("Title", "S", "Another Title"),
  ("ISBN", "S", "DEF"))

val i3 = new Item(
  ("Id", "N", "101"),
  ("Title", "S", "Tale of Two Databases"),
  ("ISBN", "S", "XYZ"))

Table.put("books_range")(i1, i2, i3)

// Range Query
val key_conditions = new KeyConditions(
    ("Id", "N", "EQ", "101", None),
    ("Title", "S", "GT", "A", None))
Table.query("books_range", key_conditions)
