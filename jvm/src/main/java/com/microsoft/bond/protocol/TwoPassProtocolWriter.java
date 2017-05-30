package com.microsoft.bond.protocol;

/**
 * Provides interface to a writer to pre-run for protocols (like Compact Binary v2) which need two passes.
 */
public interface TwoPassProtocolWriter extends ProtocolWriter {

    /**
     * Provide the first-pass writer.
     *
     * @return the first-pass writer
     */
    ProtocolWriter getFirstPassWriter();
}
