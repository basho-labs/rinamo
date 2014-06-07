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

import com.amazonaws._
import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import bootstrap.rinamo._

trait AWSHelper {
  val client: AmazonDynamoDBClient = { RinamoConsole.config }

  // enable test side logging using profile: -Ptest-debug-logging
  val logger: Logger = LoggerFactory.getLogger(this.getClass.getName)
}
object AWSHelper extends AWSHelper
