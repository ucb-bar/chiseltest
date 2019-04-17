// See LICENSE for license details.

package chisel3.tester

/** Your warranty is now void.
  *
  * experimental contains cutting edge features that are, well, experimental, and carry no
  * expectation of long-term support. We may break experimental APIs at any time. These may not
  * work as expected, or may have unforeseen side effects, or may be powerful yet dangerous.
  *
  * You have been warned.
  */
package object experimental {
  type TesterOptions = chisel3.tester.internal.TesterOptions
  val TesterOptions = chisel3.tester.internal.TesterOptions  // expose this internal object, whose "API" is unstable
}
