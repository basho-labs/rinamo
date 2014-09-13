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

package acceptance

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import com.amazonaws.auth._
import com.amazonaws.services.s3._

import bootstrap.rinamo.RinamoConsole

object S3 {
  var client = RinamoConsole.s3_config()
  def list_buckets(): List[com.amazonaws.services.s3.model.Bucket] = {
    return client.listBuckets().toList
  }
  def reset() {
    client = RinamoConsole.s3_config()
  }
}