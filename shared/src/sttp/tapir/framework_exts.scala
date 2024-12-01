package sttp.tapir

// A hack to make package private methods visible

extension [SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R](
  e: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
) {

  /** Allows you to completely replace the security input of the endpoint. */
  def withSecurityInputPublic[SI2, R2](
    input: EndpointInput[SI2]
  ): Endpoint[SI2, INPUT, ERROR_OUTPUT, OUTPUT, R & R2] =
    e.withSecurityInput(input)

  /** Allows you to completely replace the input of the endpoint. */
  def withInputPublic[I2, R2](
    input: EndpointInput[I2]
  ): Endpoint[SECURITY_INPUT, I2, ERROR_OUTPUT, OUTPUT, R & R2] =
    e.withInput(input)

  /** Allows you to completely replace the output of the endpoint. */
  def withOutputPublic[O2, R2](
    output: EndpointOutput[O2]
  ): Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, O2, R & R2] =
    e.withOutput(output)
}
