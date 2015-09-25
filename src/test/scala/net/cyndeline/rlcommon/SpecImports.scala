package net.cyndeline.rlcommon

import org.scalatest.{FunSpec, GivenWhenThen, Matchers}

/**
 * Extended by every Spec class, keeps the testrelated imports in a single class to extend.
 */
abstract class SpecImports extends FunSpec with GivenWhenThen with Matchers
