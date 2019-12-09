package org.sruby

import org.scalatest._

abstract class SRubySpec extends wordspec.AnyWordSpec with matchers.should.Matchers with
  PartialFunctionValues with PrivateMethodTester with Factories
